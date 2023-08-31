import {
  IdAliasCredentials,
  PreparedIdAlias,
} from "$generated/internet_identity_types";
import {
  AuthnTemplates,
  authenticateBox,
} from "$src/components/authenticateBox";
import { displayError } from "$src/components/displayError";
import { showMessage } from "$src/components/message";
import { toast } from "$src/components/toast";
import { KnownDapp, getDapps } from "$src/flows/dappsExplorer/dapps";
import { prompt } from "$src/flows/verifiableCredentials/prompt";
import { select } from "$src/flows/verifiableCredentials/select";
import { I18n } from "$src/i18n";
import { AuthenticatedConnection, Connection } from "$src/utils/iiConnection";
import { TemplateElement } from "$src/utils/lit-html";
import { asNonEmptyArray, unknownToString } from "$src/utils/utils";
import { HttpAgent, polling } from "@dfinity/agent";
import { IDL } from "@dfinity/candid";
import { Principal } from "@dfinity/principal";
import { nonNullish, uint8ArrayToHexString } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";

export const vcFlow = async (connection: Connection): Promise<never> => {
  const i18n = new I18n();
  // do vc flow
  const urlParams = new URLSearchParams(window.location.search);
  const dapp = urlParams.get("dapp");
  const relyingParty = getDapps().find((d) => d.name === dapp);
  if (relyingParty === undefined) {
    await displayError({
      message: "unknown dapp",
      primaryButton: "Ok",
      title: "unknown dapp",
    });
    return await new Promise((_) => {
      /* halt */
    });
  }
  const authSuccess = await authenticateBox(
    connection,
    i18n,
    authnTemplateVc({ dapp: relyingParty })
  );
  const issuers = urlParams.get("issuers");
  if (issuers === null) {
    await displayError({
      message: "no issuer",
      primaryButton: "Ok",
      title: "no issuer",
    });
    return await new Promise((_) => {
      /* halt */
    });
  }
  const issuerIdDapps: KnownDapp[] = [];
  console.log(`issuers: ${issuers}`);
  issuers.split(" ").forEach((issuer) =>
    issuerIdDapps.push(
      new KnownDapp({
        name: issuer,
        website: "issuer.ic0.app",
        logo: "nnsfront-enddapp_logo-dark.webp",
      })
    )
  );

  const providers = asNonEmptyArray(issuerIdDapps);
  if (providers === undefined) {
    await displayError({
      message: "no issuer",
      primaryButton: "Ok",
      title: "no issuer",
    });
    return await new Promise((_) => {
      /* halt */
    });
  }

  const promptResult = await prompt({
    knownDapp: relyingParty,
    userNumber: authSuccess.userNumber,
  });
  if (promptResult === "canceled") {
    await displayError({
      message: "Canceled",
      primaryButton: "Ok",
      title: "Canceled",
    });
    return await new Promise((_) => {
      /* halt */
    });
  }

  const credentialResult = await select({
    userNumber: authSuccess.userNumber,
    providers,
    relying: { dapp: relyingParty, reason: "some placeholder text" },
    verify: (provider) =>
      issue_credentials({
        connection: authSuccess.connection,
        issuerCanisterId: provider.name,
        issuerDerivationOrigin: provider.website ?? provider.name,
        relyingPartyDerivationOrigin: relyingParty.website ?? relyingParty.name,
      }),
  });

  if (credentialResult instanceof Error) {
    await displayError({
      message: "Error getting credential: " + credentialResult.message,
      primaryButton: "Ok",
      title: "Error getting credential",
    });
    return await new Promise((_) => {
      /* halt */
    });
  }

  console.log("credential Result: ", JSON.stringify(credentialResult));

  showMessage({
    message: html`<div style="overflow-wrap: anywhere;">
      <p class="t-strong">Id Alias Credential</p>
      <p>${credentialResult.idAliasCred}</p>
      <p class="t-strong">Issuer Credential</p>
      <p>${credentialResult.issuerCred}</p>
    </div>`,
  });
  // don't do the other flows below
  return await new Promise((_) => {
    /* halt */
  });
};

export const authnTemplateVc = ({
  dapp,
}: {
  dapp: KnownDapp;
}): AuthnTemplates => {
  const wrap = ({
    title,
    subtitle,
  }: {
    title: TemplateElement;
    subtitle?: TemplateElement;
  }): TemplateResult => html`
    <header class="l-stack">
      <h1 class="t-title t-title--main">${title}</h1>
      ${nonNullish(subtitle)
        ? html` <p class="t-lead l-stack">${subtitle}</p>`
        : undefined}
    </header>
  `;

  return {
    firstTime: {
      slot: wrap({
        title: html`Do VC attribute sharing flow for
          <strong class="t-strong">${dapp.name}</strong>?`,
      }),
      useExistingText: "Use existing",
      createAnchorText: "Create Internet Identity",
    },
    useExisting: {
      slot: wrap({
        title: html`Enter your <br />Internet Identity`,
        subtitle: html`to share attribute with
          <strong class="t-strong">${dapp.name}</strong>`,
      }),
    },
    pick: {
      slot: wrap({
        title: html`Choose your <br />Internet Identity`,
        subtitle: html`to share attribute with
          <strong class="t-strong">${dapp.name}</strong>`,
      }),
    },
  };
};

const issue_credentials = async ({
  connection,
  issuerCanisterId,
  issuerDerivationOrigin,
  relyingPartyDerivationOrigin,
}: {
  connection: AuthenticatedConnection;
  issuerCanisterId: string;
  issuerDerivationOrigin: string;
  relyingPartyDerivationOrigin: string;
}): Promise<{ idAliasCred: string; issuerCred: string } | Error> => {
  console.log("Preparing id alias...");
  const prepareIdAliasResponse = await prepareIdAlias(
    connection,
    issuerDerivationOrigin,
    relyingPartyDerivationOrigin
  );

  console.log("Getting id alias...");
  const idAliasCredentials = await retryGetIdAlias(
    connection,
    prepareIdAliasResponse,
    issuerDerivationOrigin,
    relyingPartyDerivationOrigin
  );

  console.log("Issuing credentials...");

  const issuerAgent = new HttpAgent({
    //identity: connection.delegationIdentity,
  });
  await issuerAgent.fetchRootKey();

  const credentialRequest = {
    signed_id_alias: idAliasCredentials.issuer_id_alias_credential,
    credential_spec: { info: "this is a test credential" },
  };

  const prepareArg = IDL.encode(
    [
      IDL.Record({
        signed_id_alias: IDL.Record({
          credential_jws: IDL.Text,
          id_alias: IDL.Principal,
          id_dapp: IDL.Principal,
        }),
        credential_spec: IDL.Record({ info: IDL.Text }),
      }),
    ],
    [credentialRequest]
  );

  console.log(
    "prepareArg: ",
    uint8ArrayToHexString(new Uint8Array(prepareArg))
  );

  const submitResponse = await issuerAgent.call(issuerCanisterId, {
    methodName: "prepare_credential",
    arg: prepareArg,
  });

  // poll read state

  const prepareCredentialResponse = await polling.pollForResponse(
    issuerAgent,
    Principal.fromText(issuerCanisterId),
    submitResponse.requestId,
    () => Promise.resolve() // nop strategy
  );

  const [prepared_credential] = IDL.decode(
    [IDL.Variant({ Ok: IDL.Record({ vc_jwt: IDL.Text }), Err: IDL.Text })],
    prepareCredentialResponse
  );

  if (
    !(
      typeof prepared_credential === "object" &&
      prepared_credential !== null &&
      "Ok" in prepared_credential &&
      typeof prepared_credential.Ok === "object" &&
      prepared_credential.Ok !== null &&
      "vc_jwt" in prepared_credential.Ok
    )
  ) {
    console.log(
      "error while preparing credential: ",
      JSON.stringify(prepared_credential)
    );
    return Promise.resolve(
      new Error("error: " + JSON.stringify(prepared_credential))
    );
  }

  console.log("Credential issued!");
  console.log("Prepared credential: ", JSON.stringify(prepared_credential));

  const getArg = IDL.encode(
    [
      IDL.Record({
        signed_id_alias: IDL.Record({
          credential_jws: IDL.Text,
          id_alias: IDL.Principal,
          id_dapp: IDL.Principal,
        }),
        vc_jwt: IDL.Text,
        credential_spec: IDL.Record({ info: IDL.Text }),
      }),
    ],
    [{ vc_jwt: prepared_credential.Ok.vc_jwt, ...credentialRequest }]
  );

  const getCredentialResponse = await issuerAgent.query(issuerCanisterId, {
    methodName: "get_credential",
    arg: getArg,
  });
  console.log("getCredentialResponse: ", getCredentialResponse.status);

  if (getCredentialResponse.status === "rejected") {
    return Promise.resolve(new Error("get_credentials rejected"));
  }

  const [getCredentialDecoded] = IDL.decode(
    [IDL.Variant({ Ok: IDL.Record({ vc_jws: IDL.Text }), Err: IDL.Text })],
    getCredentialResponse.reply.arg
  );

  if (
    !(
      typeof getCredentialDecoded === "object" &&
      getCredentialDecoded !== null &&
      "Ok" in getCredentialDecoded &&
      typeof getCredentialDecoded.Ok === "object" &&
      getCredentialDecoded.Ok !== null &&
      "vc_jws" in getCredentialDecoded.Ok
    )
  ) {
    console.log(
      "error while getting credential: ",
      JSON.stringify(getCredentialDecoded)
    );
    return Promise.resolve(
      new Error("error: " + JSON.stringify(getCredentialDecoded))
    );
  }
  return Promise.resolve({
    issuerCred: JSON.stringify(getCredentialDecoded.Ok.vc_jws),
    idAliasCred: idAliasCredentials.rp_id_alias_credential.credential_jws,
  });
};

const prepareIdAlias = async (
  connection: AuthenticatedConnection,
  issuerDerivationOrigin: string,
  relyingPartyDerivationOrigin: string
): Promise<PreparedIdAlias> => {
  const prepareIdAliasResponse = await connection.prepareIdAlias(
    issuerDerivationOrigin,
    relyingPartyDerivationOrigin
  );
  if ("error" in prepareIdAliasResponse) {
    throw prepareIdAliasResponse.error;
  }
  if ("authentication_failed" in prepareIdAliasResponse) {
    throw new Error("Error while fetching delegation: authentication_failed");
  }
  return prepareIdAliasResponse.ok;
};

const retryGetIdAlias = async (
  connection: AuthenticatedConnection,
  preparedIdAlias: PreparedIdAlias,
  issuerDerivationOrigin: string,
  relyingPartyDerivationOrigin: string,
  maxRetries = 5
): Promise<IdAliasCredentials> => {
  for (let i = 0; i < maxRetries; i++) {
    // Linear backoff
    await new Promise((resolve) => {
      setInterval(resolve, 1000 * i);
    });
    const res = await connection.getIdAlias(
      preparedIdAlias,
      issuerDerivationOrigin,
      relyingPartyDerivationOrigin
    );
    if ("no_such_credentials" in res) {
      continue;
    }

    if ("error" in res) {
      toast.error(
        "Error while fetching id alias: " +
          unknownToString(res.error, "unknown error")
      );
      continue;
    }

    if ("authentication_failed" in res) {
      throw new Error("Error while fetching delegation: authentication_failed");
    }

    return res.ok;
  }
  throw new Error(
    `Failed to retrieve a delegation after ${maxRetries} retries.`
  );
};
