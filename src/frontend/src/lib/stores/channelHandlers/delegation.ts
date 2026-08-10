import { toPermissionsArg } from "$lib/utils/accessLevel";
import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  DelegationParamsCodec,
  DelegationResultSchema,
  INTERACTION_REQUIRED_ERROR_CODE,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import {
  authorizationPromptStore,
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import {
  type AppDelegationRecord,
  appDelegationsForOrigin,
  discardAppDelegation,
  storeAppDelegation,
} from "$lib/stores/app-delegation.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { readMultipleAccountsToggle } from "$lib/utils/multipleAccounts";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import {
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
  waitForStore,
} from "$lib/utils/utils";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  attributeConsentResultStore,
  attributeConsentStore,
} from "$lib/stores/attributeConsent.store";
import { get } from "svelte/store";

/** Why a delegation could not be re-issued without asking the user something.
 *  Reported to the app as `data.reason`, and named after OpenID Connect's
 *  equivalents since `?prompt=` borrows from it. */
export type SilentDenial = "login_required" | "account_selection_required";

type SilentOutcome = { record: AppDelegationRecord } | { denial: SilentDenial };

/**
 * Picks the delegation to re-issue with no user interaction, or explains why
 * there isn't one.
 *
 * Only safe when there is exactly one delegation the user could have meant. Two
 * things could be ambiguous, and a `hint` settles both at once because a
 * principal identifies an identity and an account together.
 *
 * Ambiguity is judged from the identities this device knows, not from the stored
 * records: the records only cover origins the user has already signed in to, so
 * gating on them would silently sign someone in as their only cached identity
 * when they have another they might have wanted. Accounts are judged from the
 * multiple-accounts toggle rather than the account count, because with the
 * toggle off Internet Identity never offers an account choice at all, so
 * answering silently withholds nothing the user was being shown.
 */
export const chooseSilentDelegation = ({
  records,
  hint,
  identityCount,
  isMultipleAccountsEnabled,
}: {
  /** Usable (unexpired) records for the effective origin. */
  records: AppDelegationRecord[];
  hint: string | undefined;
  /** Identities in the last-used list on this device. */
  identityCount: number;
  isMultipleAccountsEnabled: (identityNumber: bigint) => boolean;
}): SilentOutcome => {
  if (records.length === 0) {
    return { denial: "login_required" };
  }

  if (hint !== undefined) {
    const hinted = records.find((record) => record.principal === hint);
    // A hint naming a principal with nothing stored for this origin is the same
    // situation as nothing being stored at all: the app has to sign in.
    return hinted !== undefined
      ? { record: hinted }
      : { denial: "login_required" };
  }

  if (identityCount !== 1 || records.length !== 1) {
    return { denial: "account_selection_required" };
  }

  const [record] = records;
  if (isMultipleAccountsEnabled(record.identityNumber)) {
    return { denial: "account_selection_required" };
  }

  return { record };
};

/** Serialize delegation requests so a malicious dapp sending several in
 *  parallel can't race the authorization state (effective origin, auth
 *  flow, authorized account) against itself. */
let delegationQueueTail: Promise<unknown> = Promise.resolve();
const serializeDelegationRequest = <T>(fn: () => Promise<T>): Promise<T> => {
  const prev = delegationQueueTail;
  const next = prev.then(fn);
  delegationQueueTail = next.catch(() => {});
  return next;
};

/**
 * ICRC-34: handle a delegation request from the relying party.
 *
 * Drives the authorization flow by validating the request, exposing context
 * to the UI, waiting for the user to authorize, and sending the delegation back.
 */
export const handleDelegationRequest =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "icrc34_delegation") {
      return;
    }
    const requestId = request.id;

    const result = DelegationParamsCodec.safeParse(request.params);
    if (!result.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(result.error),
        },
      });
      onError("invalid-request");
      return;
    }

    await serializeDelegationRequest(async () => {
      try {
        const params = result.data;

        const validationResult = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: params.icrc95DerivationOrigin,
        });
        if (validationResult.result === "invalid") {
          onError("unverified-origin");
          return;
        }

        // Compute effective origin (derivation origin if provided, else
        // channel origin) and remap *.icp0.io to *.ic0.app for legacy
        // compatibility.
        const effectiveOrigin = remapToLegacyDomain(
          params.icrc95DerivationOrigin ?? channel.origin,
        );

        // Read from the store rather than the URL: the params were captured and
        // stripped when the page loaded, and this handler runs again on a flow
        // resumed after an identity provider round-trip, when the address bar no
        // longer has them.
        const { prompt, hint } = get(authorizationPromptStore);

        // Re-issue from a delegation this frontend already holds for the origin,
        // so an app that has signed in before does not spend another passkey.
        // Deliberately ahead of `setRequestContext`: that is what makes the
        // sign-in UI render, and a silent answer must not paint anything.
        let denial: SilentDenial | undefined;
        if (prompt !== "login") {
          const outcome = chooseSilentDelegation({
            records: await appDelegationsForOrigin(effectiveOrigin),
            hint,
            identityCount: Object.keys(get(lastUsedIdentitiesStore).identities)
              .length,
            isMultipleAccountsEnabled: readMultipleAccountsToggle,
          });
          if ("record" in outcome) {
            try {
              const { keyPair, chainJson, expiresAtMillis } = outcome.record;
              const chain = await DelegationChain.create(
                await ECDSAKeyIdentity.fromKeyPair(keyPair),
                params.publicKey,
                // Expires with its parent rather than later: a chain is only
                // valid until its earliest expiry, so matching it means
                // re-issuing restores a session without ever lengthening it
                // past what the user originally granted.
                new Date(expiresAtMillis),
                { previous: DelegationChain.fromJSON(JSON.parse(chainJson)) },
              );
              await channel.send({
                jsonrpc: "2.0",
                id: requestId,
                result: DelegationResultSchema.encode(chain),
              });
              return;
            } catch (error) {
              // The record did not survive storage (its key pair or chain is
              // unusable). Drop it and sign in the ordinary way rather than
              // failing a request a ceremony can still answer.
              console.error(error);
              await discardAppDelegation(
                effectiveOrigin,
                outcome.record.principal,
              );
              denial = "login_required";
            }
          } else {
            denial = outcome.denial;
          }
        }

        if (prompt === "none") {
          // No `onError` here: it sets `channelErrorStore`, which renders the
          // channel-error view — the interstitial `prompt=none` exists to
          // promise the user will never be shown. Answering on the channel lets
          // the app close the popup, or the redirect return, and decide for
          // itself whether to escalate to an interactive request.
          await channel.send({
            jsonrpc: "2.0",
            id: requestId,
            error: {
              code: INTERACTION_REQUIRED_ERROR_CODE,
              message: "Interaction required",
              data: { reason: denial ?? "login_required" },
            },
          });
          return;
        }

        // Set the effective origin (which makes the sign-in UI render) and the
        // app's requested session duration together, so the sign-in screen
        // always sees the requested duration — the picker's ceiling — from its
        // first render. `undefined` when the app didn't specify one, in which
        // case the backend applies its default.
        authorizationStore.setRequestContext(
          effectiveOrigin,
          params.maxTimeToLive,
        );

        let authorized = await waitForStore(authorizedStore);
        while (
          get(attributeConsentStore) !== undefined &&
          get(attributeConsentResultStore) === undefined
        ) {
          const outcome = await Promise.race([
            waitForStore(attributeConsentResultStore).then(
              () => "settled" as const,
            ),
            waitForStore(authorizedStore, (current) =>
              current !== authorized ? ("switched" as const) : undefined,
            ),
          ]);
          if (outcome === "settled") {
            break;
          }
          authorized = await waitForStore(authorizedStore);
        }

        // Read the identity *after* authorization so we capture whichever
        // identity the user settled on (they may have switched mid-flow).
        const [accountNumber, { identityNumber, actor, authMethod }] =
          await Promise.all([
            authorized.accountNumberPromise,
            waitForStore(authenticationStore),
          ]);
        const ssoSessionMaxAgeNs =
          "openid" in authMethod
            ? authMethod.openid.ssoSessionMaxAgeNs
            : undefined;

        const sessionPublicKey = new Uint8Array(params.publicKey.toDer());

        // When the user chose "Questions only" during authorization, the
        // delegation is restricted to query calls via its `permissions`
        // field, which the IC enforces (update calls are rejected).
        //
        // The restricted delegation is now carried back to the relying party
        // intact: the encoded ICRC-34 result includes the `permissions` field
        // (see `DelegationResultSchema`), and `@icp-sdk/core` (>= 6) can
        // represent it on a `Delegation` instance. `permissions` is a
        // non-standard ICRC-34 extension, though, so the relying party's own
        // signer/client must also read it out of the delegation result and
        // pass it into the `Delegation` it reconstructs; only then does it
        // recompute the same canister-signed hash and the signature verify.
        // Send an explicit value rather than relying on the backend's
        // omitted-arg default.
        const permissions = toPermissionsArg(authorized.accessLevel);

        // Prefer the duration the user chose on the sign-in screen; it's already
        // capped at the app's request. Flows without a picker (e.g. 1-click
        // OpenID/SSO) fall back to the app's requested value. An SSO
        // organization also caps how long its sign-ins stay valid, and the
        // delegation must not outlive that, so an SSO session sends a duration
        // even when neither the picker nor the app asked for one. The backend
        // applies its own default only when nothing constrains it at all.
        const requested = authorized.maxTimeToLive ?? params.maxTimeToLive;
        const maxTimeToLive =
          ssoSessionMaxAgeNs !== undefined &&
          (requested === undefined || requested > ssoSessionMaxAgeNs)
            ? ssoSessionMaxAgeNs
            : requested;

        // Have the canister delegate to a key this frontend holds, then extend
        // that to the app's session key below. Storing the pair is what lets a
        // later request skip the ceremony, and routing through it costs no extra
        // canister call. When the key cannot be created the delegation goes
        // straight to the app's key as before, simply without a cache.
        // Non-extractable so the private half cannot be read back out of
        // storage, which also means it can only be stored by structured clone. A
        // browser that refuses either is a reason to sign in without a cache,
        // not a reason to fail the sign-in.
        const ownKey = await ECDSAKeyIdentity.generate({
          extractable: false,
        }).catch(() => undefined);
        const delegationTarget =
          ownKey === undefined
            ? sessionPublicKey
            : new Uint8Array(ownKey.getPublicKey().toDer());

        const { user_key, expiration } = await actor
          .prepare_account_delegation(
            identityNumber,
            effectiveOrigin,
            accountNumber !== undefined ? [accountNumber] : [],
            delegationTarget,
            maxTimeToLive !== undefined ? [maxTimeToLive] : [],
            permissions,
          )
          .then(throwCanisterError);

        const canisterChain = await retryFor(5, () =>
          actor
            .get_account_delegation(
              identityNumber,
              effectiveOrigin,
              accountNumber !== undefined ? [accountNumber] : [],
              delegationTarget,
              expiration,
              permissions,
            )
            .then(throwCanisterError)
            .then(transformSignedDelegation)
            .then((delegation) =>
              DelegationChain.fromDelegations(
                [delegation],
                new Uint8Array(user_key),
              ),
            ),
        );

        let delegationChain = canisterChain;
        if (ownKey !== undefined) {
          // Truncating (rather than rounding) keeps the record's expiry at or
          // before the canister's, never after it.
          const expiresAtMillis = Number(expiration / BigInt(1_000_000));
          // Awaited rather than fired off: on the redirect transport the send
          // below navigates the tab away, which would abandon a pending write.
          await storeAppDelegation(effectiveOrigin, {
            principal: Principal.selfAuthenticating(
              new Uint8Array(user_key),
            ).toText(),
            identityNumber,
            accountNumber,
            keyPair: ownKey.getKeyPair(),
            chainJson: JSON.stringify(canisterChain.toJSON()),
            expiresAtMillis,
          });
          delegationChain = await DelegationChain.create(
            ownKey,
            params.publicKey,
            new Date(expiresAtMillis),
            { previous: canisterChain },
          );
        }

        await channel.send({
          jsonrpc: "2.0",
          id: requestId,
          result: DelegationResultSchema.encode(delegationChain),
        });
      } catch (error) {
        console.error(error);
        onError("delegation-failed");
      }
    });
  };
