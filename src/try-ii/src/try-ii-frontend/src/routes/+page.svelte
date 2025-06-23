<script lang="ts">
  import Button from "$lib/components/Button.svelte";
  import { authWithII } from "$lib/authFunctions";
  import { identity, localIdentity } from "$lib/state/identity";
  import Badge from "$lib/components/Badge.svelte";
  import { ArrowRightIcon, RotateCcwIcon } from "@lucide/svelte";
  import ButtonOrAnchor from "$lib/components/ButtonOrAnchor.svelte";

  const handleSignin = async (beta?: boolean) => {
    try {
      const result = await authWithII({
        url: beta ? "https://beta.id.ai" : "https://id.ai",
        sessionIdentity: $localIdentity,
      });
      console.log(result);
      $identity = result.identity;
    } catch (e) {
      console.error(e);
    }
  };

  const handleSignOut = () => {
    $identity = undefined;
  };
</script>

<!-- <main class="h-[100dvh] overflow-hidden"> -->
<div class="flex justify-center items-center flex-col flex-1">
  {#if $identity === undefined}
    <div
      class="flex flex-col gap-6 my-4 items-center flex-1 justify-center py-16"
    >
      <h1 class="text-text-primary font-medium text-4xl text-center">
        Internet Identity <span class="text-text-secondary">v2</span>
      </h1>
      <p
        class="text-lg text-text-secondary px-8 max-w-160 text-balance text-center mb-2"
      >
        Experience next-gen decentralized authentication with enhanced features
        that make signing in easier than ever.
      </p>
      <div class="flex flex-col gap-2 justify-center items-center">
        <Button onclick={() => handleSignin()} size="xl" class="transition-all">
          <span>Try the new sign-in experience</span>
          <ArrowRightIcon size="1.25rem" />
        </Button>
        <ButtonOrAnchor
          onclick={() => handleSignin(true)}
          class="text-text-secondary box-border flex items-center justify-center justify-self-start whitespace-nowrap opacity-100 gap-2.5 transition-all hover:underline decoration-1 w-fit text-sm"
        >
          <span>Try beta</span>
          <ArrowRightIcon size="1rem" />
        </ButtonOrAnchor>
      </div>
    </div>
  {:else}
    <div
      class="flex flex-col gap-6 my-4 items-center flex-1 justify-center py-16"
    >
      <Badge size="sm" class="mx-6 px-4 py-2"
        ><span class="font-bold text-text-primary mr-1">Principal:</span><span
          >{$identity.getPrincipal().toText()}</span
        ></Badge
      >
      <h1 class="text-text-primary font-medium text-4xl text-center">
        You're In. Seamlessly and Securely
      </h1>
      <p
        class="text-lg text-text-secondary px-8 max-w-160 text-balance text-center mb-2"
      >
        You just signed in with next-gen decentralized authentication for
        faster, safer access without passwords or friction.
      </p>
      <Button size="xl" variant="secondary" onclick={handleSignOut}>
        <RotateCcwIcon size="1.25rem" />
        <span>Sign out to try again</span>
      </Button>
    </div>
  {/if}
  <div class="flex gap-8 px-8 md:flex-row flex-col max-w-320 mb-16">
    <div
      class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start"
    >
      <Badge size="lg" class="mb-3">Simplified Access</Badge>
      <h2 class="text-xl text-text-primary mb-2 font-medium">
        Discoverable Passkeys
      </h2>
      <p class="text-md text-text-secondary">
        No more remembering identity numbers. Simply select your name from the
        list thanks to discoverable passkeys.
      </p>
    </div>
    <div
      class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start"
    >
      <Badge size="lg" class="mb-3">Full Control</Badge>
      <h2 class="text-xl text-text-primary mb-2 font-medium">
        Multiple Accounts
      </h2>
      <p class="text-md text-text-secondary">
        Use a single identity to create and manage multiple accounts on the same
        dapp, giving you more flexibility and control.
      </p>
    </div>
    <div
      class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start"
    >
      <Badge size="lg" class="mb-3">Low Friction</Badge>
      <h2 class="text-xl text-text-primary mb-2 font-medium">
        Sign in with Google
      </h2>
      <p class="text-md text-text-secondary">
        Easily onboard web2 users by securely connecting their Google accounts,
        combining familiar access with the power of decentralized identity.
      </p>
    </div>
  </div>
</div>
<!-- </main> -->
