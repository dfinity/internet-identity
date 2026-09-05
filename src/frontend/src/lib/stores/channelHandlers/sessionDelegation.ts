import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  Base64ToBytesCodec,
  Base64ToPublicKeyCodec,
  INTERACTION_REQUIRED_ERROR_CODE,
  INVALID_PARAMS_ERROR_CODE,
  OriginSchema,
  StringToBigIntCodec,
} from "$lib/utils/transport/utils";
import {
  authorizationPromptStore,
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  appSessionsForOrigin,
  discardAppSession,
  rememberAppAccount,
  storeAppSession,
  type AppSessionRecord,
} from "$lib/stores/app-session.store";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import { toPermissionsArg } from "$lib/utils/accessLevel";
import { retryFor, throwCanisterError, waitForStore } from "$lib/utils/utils";
import { agentOptions, canisterId } from "$lib/globals";
import { Actor, HttpAgent } from "@icp-sdk/core/agent";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { Principal } from "@icp-sdk/core/principal";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import type { PublicKey, Signature } from "@icp-sdk/core/agent";
import { get } from "svelte/store";
import {
  chooseSilentSession,
  type SilentDenial,
} from "../../../routes/(new-styling)/authorize/silentReauth";
import { serializeAuthorizationRequest } from "$lib/stores/channelHandlers/serialize";
import { withBrowserProof } from "$lib/stores/browser-key.store";
import { describeBrowser } from "$lib/stores/channelHandlers/describeBrowser";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";

export const SESSION_DELEGATION_METHOD = "ii_session_delegation";

const SessionParamsCodec = z.object({
  sessionPublicKey: Base64ToPublicKeyCodec,
  // How long the app is willing for the session to last. A ceiling rather than a
  // request: what the user picks at consent wins, an SSO organization's cap
  // narrows it further, and the canister clamps the result.
  maxTimeToLive: z.optional(StringToBigIntCodec),
  // How long the session may go unminted before the canister ends it. A ceiling
  // like `maxTimeToLive`: the canister clamps it to between 10 minutes and the
  // session's own granted length, and applies its own default where absent.
  maxTimeToIdle: z.optional(StringToBigIntCodec),
  icrc95DerivationOrigin: z.optional(OriginSchema),
});

/**
 * Unlike the ICRC-34 result, this one carries `targets`. The session chain is restricted
 * to the II canister, so an app that reaches for it where it meant its app delegation
 * fails immediately and visibly instead of appearing to work.
 */
const SessionResultSchema = z.codec(
  z.object({
    publicKey: z.base64(),
    signerDelegation: z.array(
      z.object({
        delegation: z.object({
          pubkey: z.base64(),
          expiration: z.string(),
          targets: z.optional(z.array(z.string())),
        }),
        signature: z.base64(),
      }),
    ),
  }),
  z.custom<{
    chain: DelegationChain;
  }>(),
  {
    decode: ({ publicKey, signerDelegation }) => ({
      chain: DelegationChain.fromDelegations(
        signerDelegation.map(
          ({ delegation: { pubkey, expiration, targets }, signature }) => ({
            delegation: new Delegation(
              Base64ToBytesCodec.decode(pubkey),
              StringToBigIntCodec.decode(expiration),
              targets?.map((target) => Principal.fromText(target)),
            ),
            signature: Base64ToBytesCodec.decode(signature) as Signature,
          }),
        ),
        Base64ToBytesCodec.decode(publicKey),
      ),
    }),
    encode: ({ chain }) => ({
      publicKey: Base64ToBytesCodec.encode(
        new Uint8Array(chain.publicKey) as Uint8Array<ArrayBuffer>,
      ),
      signerDelegation: chain.delegations.map((signed) => ({
        delegation: {
          pubkey: Base64ToBytesCodec.encode(
            new Uint8Array(signed.delegation.pubkey) as Uint8Array<ArrayBuffer>,
          ),
          expiration: signed.delegation.expiration.toString(),
          targets: signed.delegation.targets?.map((target) => target.toText()),
        },
        signature: Base64ToBytesCodec.encode(
          new Uint8Array(signed.signature) as Uint8Array<ArrayBuffer>,
        ),
      })),
    }),
  },
);

const extendToApp = async (
  record: AppSessionRecord,
  appPublicKey: PublicKey,
): Promise<DelegationChain> =>
  DelegationChain.create(
    await ECDSAKeyIdentity.fromKeyPair(record.keyPair),
    appPublicKey,
    new Date(record.expiresAtMillis),
    {
      previous: DelegationChain.fromJSON(JSON.parse(record.chainJson)),
      targets: [Principal.from(canisterId)],
    },
  );

/**
 * Whether the canister still holds the session this record names.
 *
 * A record can outlive its session: revoking from settings or from another app leaves
 * this browser's copy in place. Answering from the record alone would hand the app a
 * chain that cannot mint, and the failure would surface later as something the client
 * cannot tell apart from a real error.
 */
const sessionIsLive = async (record: AppSessionRecord): Promise<boolean> => {
  try {
    const identity = DelegationIdentity.fromDelegation(
      await ECDSAKeyIdentity.fromKeyPair(record.keyPair),
      DelegationChain.fromJSON(JSON.parse(record.chainJson)),
    );
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent: HttpAgent.createSync({ ...agentOptions, identity }),
      canisterId,
    });
    return await actor.check_session();
  } catch {
    return false;
  }
};

/**
 * Obtains the session an app re-issues its own delegations from.
 *
 * The response carries a session and nothing else: the app mints its first app
 * delegation through `app_prepare_delegation`, the same call it uses for every
 * subsequent one, so `icrc34_delegation` keeps behaving exactly as it does today and an
 * app that cannot refresh simply never calls this.
 */
export const handleSessionDelegationRequest =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (
      request.id === undefined ||
      request.method !== SESSION_DELEGATION_METHOD
    ) {
      return;
    }
    const requestId = request.id;

    const isSilent = get(authorizationPromptStore).prompt === "none";
    const deny = async (reason: SilentDenial) => {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INTERACTION_REQUIRED_ERROR_CODE,
          message: "Interaction required",
          data: { reason },
        },
      });
    };

    const parsed = SessionParamsCodec.safeParse(request.params);
    if (!parsed.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(parsed.error),
        },
      });
      // A malformed request is still a protocol error rather than a denial, so the code
      // stays INVALID_PARAMS. What the silent path must not do is render: it was asked to
      // answer without showing the user anything, and that holds however it fails.
      if (!isSilent) {
        onError("invalid-request");
      }
      return;
    }

    await serializeAuthorizationRequest(async () => {
      try {
        const params = parsed.data;
        const validation = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: params.icrc95DerivationOrigin,
        });
        if (validation.result === "invalid") {
          if (isSilent) {
            await deny("login_required");
            return;
          }
          onError("unverified-origin");
          return;
        }

        const effectiveOrigin = remapToLegacyDomain(
          params.icrc95DerivationOrigin ?? channel.origin,
        );

        const { prompt, hint, resumable } = get(authorizationPromptStore);
        // Silence is something an app asks for. Anything else, an absent `prompt` included,
        // runs the ceremony, so a held session is never handed over without the user
        // seeing a screen they did not request.
        const held =
          prompt === "none" ? await appSessionsForOrigin(effectiveOrigin) : [];
        const chosen = chooseSilentSession({ held, hint });

        let usable = "session" in chosen ? chosen.session : undefined;
        if (usable && !(await sessionIsLive(usable.record))) {
          await discardAppSession({
            identityNumber: usable.identityNumber,
            accountNumber: usable.accountNumber,
            origin: effectiveOrigin,
          });
          usable = undefined;
        }

        if (usable) {
          const chain = await extendToApp(
            usable.record,
            params.sessionPublicKey,
          );
          await channel.send({
            jsonrpc: "2.0",
            id: requestId,
            result: SessionResultSchema.encode({
              chain,
            }),
          });
          return;
        }

        if (isSilent) {
          await deny("denial" in chosen ? chosen.denial : "login_required");
          return;
        }

        const created = await createSession(
          effectiveOrigin,
          params.maxTimeToLive,
          params.maxTimeToIdle,
          resumable === true,
        );
        const chain = await extendToApp(
          created.record,
          params.sessionPublicKey,
        );
        await channel.send({
          jsonrpc: "2.0",
          id: requestId,
          result: SessionResultSchema.encode({
            chain,
          }),
        });
      } catch (error) {
        console.error(error);
        if (isSilent) {
          await deny("login_required");
          return;
        }
        onError("delegation-failed");
      }
    });
  };

const createSession = async (
  effectiveOrigin: string,
  requestedMaxTimeToLive: bigint | undefined,
  requestedMaxTimeToIdle: bigint | undefined,
  resumable: boolean,
): Promise<{ record: AppSessionRecord }> => {
  authorizationStore.setRequestContext(effectiveOrigin, requestedMaxTimeToLive);
  const authorized = await waitForStore(authorizedStore);
  const [accountNumber, { identityNumber, actor, authMethod }] =
    await Promise.all([
      authorized.accountNumberPromise,
      waitForStore(authenticationStore),
    ]);
  // An SSO organization caps how long its sign-ins stay valid, and a session must not
  // outlive that, so an SSO identity sends a duration even when the user picked none.
  const ssoSessionMaxAgeNs =
    "openid" in authMethod ? authMethod.openid.ssoSessionMaxAgeNs : undefined;
  // What the user picked wins over what the app asked for; the app's value is the
  // ceiling that applies when the picker offered nothing.
  const requested = authorized.maxTimeToLive ?? requestedMaxTimeToLive;
  const validFor =
    ssoSessionMaxAgeNs !== undefined &&
    (requested === undefined || requested > ssoSessionMaxAgeNs)
      ? ssoSessionMaxAgeNs
      : requested;

  const key = { identityNumber, accountNumber, origin: effectiveOrigin };
  const iiKey = await ECDSAKeyIdentity.generate({ extractable: false });
  const iiPublicKey = new Uint8Array(iiKey.getPublicKey().toDer());
  const deviceName = await describeBrowser();

  const prepared = await withBrowserProof(
    identityNumber,
    iiPublicKey,
    async (browser) => {
      const prepared = await actor
        .prepare_account_session({
          identity_number: identityNumber,
          origin: effectiveOrigin,
          account_number: accountNumber !== undefined ? [accountNumber] : [],
          session_key: iiPublicKey,
          device_name: deviceName,
          current_device_key: browser.publicKey,
          next_device_key: browser.nextPublicKey,
          current_device_key_signature: browser.signature,
          next_device_key_signature: browser.nextSignature,
          permissions: toPermissionsArg(authorized.accessLevel),
          // The duration the user chose at consent, clamped by the canister. Dropping it
          // would honour half of a consent and silently discard the other half.
          valid_for: validFor !== undefined ? [validFor] : [],
          // Straight through: the bound is the app's to ask for and the
          // canister's to clamp, and nothing at consent narrows it.
          max_idle:
            requestedMaxTimeToIdle !== undefined
              ? [requestedMaxTimeToIdle]
              : [],
        })
        .then(throwCanisterError);
      await browser.accept(prepared.device_id);
      return prepared;
    },
  );

  const fetched = await retryFor(5, () =>
    actor
      .get_account_session({
        identity_number: identityNumber,
        origin: effectiveOrigin,
        account_number: accountNumber !== undefined ? [accountNumber] : [],
        session_key: iiPublicKey,
        device_id: prepared.device_id,
        created_at: prepared.created_at,
        expiration: prepared.expiration,
      })
      .then(throwCanisterError),
  );

  const canisterChain = DelegationChain.fromDelegations(
    [
      {
        delegation: new Delegation(
          new Uint8Array(fetched.signed_delegation.delegation.pubkey),
          fetched.signed_delegation.delegation.expiration,
        ),
        signature: new Uint8Array(
          fetched.signed_delegation.signature,
        ) as Signature,
      },
    ],
    new Uint8Array(prepared.user_key),
  );

  const record: AppSessionRecord = {
    keyPair: iiKey.getKeyPair(),
    chainJson: JSON.stringify(canisterChain.toJSON()),
    expiresAtMillis: Number(prepared.expiration / BigInt(1_000_000)),
    createdAtNanos: prepared.created_at,
    accessLevel: authorized.accessLevel,
  };
  // The mapping is not a credential and is kept either way, so a later hint still names
  // an account this browser has seen. The session is what an app has to ask to have kept.
  await rememberAppAccount(key, {
    accountPrincipal: prepared.account_principal.toText(),
  });
  if (resumable) {
    await storeAppSession(key, record);
  }
  return { record };
};
