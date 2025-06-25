<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import {
    AppleIcon,
    InfoIcon,
    Plus,
    TriangleAlertIcon,
    Unlink,
  } from "@lucide/svelte";
  import ListItem from "$lib/components/ui/ListItem.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import { canisterConfig } from "$lib/globals";
  import {
    createAnonymousNonce,
    createGoogleRequestConfig,
    decodeJWT,
    decodeJWTWithNameAndEmail,
    requestJWT,
  } from "$lib/utils/openID";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { isNullish } from "@dfinity/utils";
  import { type OpenIdCredential } from "$lib/generated/internet_identity_types";

  let removableOpenIdCredential = $state<OpenIdCredential | null>(null);
  let displayAddCredentialDialog = $state(false);

  const handleAddGoogle = async () => {
    const googleClientId = canisterConfig.openid_google[0]?.[0]?.client_id;
    if (isNullish(googleClientId)) throw new Error("Missing Google client ID");
    const { nonce, salt } = await createAnonymousNonce(
      $authenticatedStore.identity.getPrincipal(),
    );
    const jwt = await requestJWT(createGoogleRequestConfig(googleClientId), {
      mediation: "required",
      nonce,
    });

    const { iss, sub, aud, name, email } = decodeJWTWithNameAndEmail(jwt);

    if (
      identityInfo.openIdCredentials.find((c) => c.iss === iss && c.sub === sub)
    ) {
      throw new Error("Account already linked");
    }

    const googleAddPromise = $authenticatedStore.actor.openid_credential_add(
      $authenticatedStore.identityNumber,
      jwt,
      salt,
    );
    // Optimistically show as added
    identityInfo.openIdCredentials.push({
      aud,
      iss,
      sub,
      metadata: [
        ["name", { String: name }],
        ["email", { String: email }],
      ],
      last_usage_timestamp: [],
    });
    displayAddCredentialDialog = false;

    const googleAddResult = await googleAddPromise;

    if ("Ok" in googleAddResult) {
      identityInfo.fetch();
    } else {
      identityInfo.openIdCredentials = identityInfo.openIdCredentials.filter(
        (cred) => !(cred.iss === iss && cred.sub === sub),
      );
      throw new Error(Object.keys(await googleAddResult.Err)[0]);
    }
  };

  const handleRemoveGoogle = async (credential: OpenIdCredential) => {
    const googleRemovePromise =
      $authenticatedStore.actor.openid_credential_remove(
        $authenticatedStore.identityNumber,
        [credential.iss, credential.sub],
      );
    // Optimistically show as removed
    identityInfo.openIdCredentials = identityInfo.openIdCredentials.filter(
      (cred) => !(cred.iss === credential.iss && cred.sub === credential.sub),
    );
    removableOpenIdCredential = null;

    const googleRemoveResult = await googleRemovePromise;

    if ("Ok" in googleRemoveResult) {
      identityInfo.fetch();
    } else {
      identityInfo.openIdCredentials.push(credential);
      throw new Error(Object.keys(googleRemoveResult.Err)[0]);
    }
  };
</script>

<div class="text-text-primary">
  <h1 class="mb-4 text-4xl">Security</h1>
  <h2 class="mb-12 text-lg">
    Settings and recommendations to keep your identity secure
  </h2>
  <Panel>
    <div class="flex flex-col justify-between gap-5 p-4 md:flex-row">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4>Manage your passkeys, security keys, and linked accounts.</h4>
      </div>

      <div>
        <Button
          onclick={() => {
            displayAddCredentialDialog = true;
          }}
          class="bg-bg-brand-solid text-text-primary-inversed text-[] top-0 flex w-full items-center justify-center gap-1 rounded-sm px-3.5 py-2 font-semibold md:max-w-fit"
          >Add <Plus size="1.25rem" /></Button
        >
      </div>
    </div>
    <ul>
      {#each identityInfo.devices as device}
        <ListItem>
          <div class="min-w-8">
            <AppleIcon />
          </div>
          <div class="flex-1">
            <AccessMethod accessMethod={device} />
          </div>
          <!-- for layout consistency -->
          <!-- TODO: this is where we would add interactions like removal -->
          <div class="w-[52px]"></div>
        </ListItem>
      {/each}
      {#each identityInfo.openIdCredentials as credential}
        <ListItem>
          <div class="min-w-8">
            <GoogleIcon />
          </div>
          <div class="flex-1">
            <AccessMethod accessMethod={credential} />
          </div>

          <Button
            variant="tertiary"
            onclick={() => (removableOpenIdCredential = credential)}
          >
            <Unlink class="stroke-fg-error-secondary" />
          </Button>
        </ListItem>
      {/each}
    </ul>
  </Panel>
</div>

{#if removableOpenIdCredential}
  <Dialog onClose={() => (removableOpenIdCredential = null)}>
    <FeaturedIcon class="mb-3" variant="warning"
      ><TriangleAlertIcon /></FeaturedIcon
    >
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Are you sure?</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      You're about to unlink your Google Account. If you proceed, you will no
      longer be able to sign-in to your identity or dapps using your Google
      Account.
    </p>
    <div class="flex w-full flex-col gap-3">
      <Button
        onclick={() => handleRemoveGoogle(removableOpenIdCredential!)}
        variant="primary"
        danger>Unlink Google Account</Button
      >
      <Button variant="tertiary">Keep linked</Button>
    </div>
  </Dialog>
{/if}

{#if displayAddCredentialDialog}
  <Dialog onClose={() => (displayAddCredentialDialog = false)}>
    <FeaturedIcon class="mb-3" variant="info"><InfoIcon /></FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Add credential</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      Please choose the type of credential you would like to add.
    </p>
    <div class="flex w-full flex-col gap-3">
      <!-- TODO: if/when we add more credentials and OpenID providers, we'll need to add more buttons here -->
      <Button variant="primary" onclick={handleAddGoogle}
        ><GoogleIcon /> Link Google Account</Button
      >
      <Button variant="tertiary">Cancel</Button>
    </div>
  </Dialog>
{/if}
