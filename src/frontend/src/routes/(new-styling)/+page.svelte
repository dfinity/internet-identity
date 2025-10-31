<script lang="ts">
  import Footer from "$lib/components/layout/Footer.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import TextFade from "$lib/components/ui/TextFade.svelte";
  import FullControlIllustration from "$lib/components/illustrations/landing/FullControlIllustration.svelte";
  import EasyAccessIllustration from "$lib/components/illustrations/landing/EasyAccessIllustration.svelte";
  import PasswordFreeIllustration from "$lib/components/illustrations/landing/PasswordFreeIllustration.svelte";
  import LandingCard from "$lib/components/ui/LandingCard.svelte";
  import {
    INTERNET_COMPUTER_URL,
    FAQ_PASSKEY_URL,
    II_DEVELOPER_DOCS_URL,
  } from "$lib/config";
  import LandingHeader from "$lib/components/layout/LandingHeader.svelte";
  import { manuallyReroute } from "../../hooks";
  import { t } from "$lib/stores/locale.store";
  import { LanguageSelector, Trans } from "$lib/components/locale";
  import { MinusCircleIcon, PlusCircleIcon } from "@lucide/svelte";
  import FlairCanvas from "$lib/components/backgrounds/FlairCanvas.svelte";
  import { DROP_WAVE_ANIMATION } from "$lib/components/backgrounds/constants";
  import type { FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

  let triggerAnimation =
    $state<(opts: FlairAnimationOptions) => Promise<void>>();
  let clearAnimation = $state<() => void>();

  // Add rerouting back on this SSG route
  $effect(() => {
    manuallyReroute();
  });

  $effect(() => {
    triggerAnimation?.(DROP_WAVE_ANIMATION);
    return () => {
      clearAnimation?.();
    };
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="sm absolute top-0 right-0 left-0 -z-1 h-[640px] max-md:hidden">
    <!-- Render animation specific to landing page here instead of using global instance -->
    <FlairCanvas
      spacing="medium"
      aspect="ultrawide"
      dotSize="small"
      vignette="none"
      visibility="maskwave"
      maskWaveRampIn={0.001}
      maskWaveRampOut={0.5}
      maskWaveThickness="large"
      maskWaveMinValue={0}
      maskWaveSpeedMultiplier={2}
      maskWavePauseValue={0.25}
      maskWaveOneWay={true}
      enableRandomPointSize
      enableRandomOpacity={false}
      pointSizeNoiseScale="medium"
      pointSizeNoiseMultiplier="medium"
      springOrTween={{
        type: "spring",
        stiffness: "medium",
        dampening: "medium",
      }}
      containerHeight="h-[640px]"
      bind:triggerAnimation
      bind:clearAnimation
    />
    <!-- This div fades out the bottom end of the background wave animation -->
    <div
      class="from-bg-primary pointer-events-none absolute right-0 bottom-0 left-0 h-[200px] bg-gradient-to-t to-transparent"
    ></div>
  </div>
  <div class="h-[env(safe-area-inset-top)]"></div>
  <LandingHeader class="w-full flex-col md:flex-row">
    <div
      class="border-border-secondary flex w-full flex-1 flex-row items-center justify-center gap-5 border-y py-3 md:justify-end md:border-0"
    >
      <LanguageSelector />
      <Button variant="secondary" href={II_DEVELOPER_DOCS_URL} target="_blank">
        {$t`For developers`}
      </Button>
      <Button variant="primary" href="/login">{$t`Manage Identity`}</Button>
    </div>
  </LandingHeader>
  <div class="flex h-[392px] w-full flex-row px-4 sm:h-[512px]">
    <div
      class="fade-in flex w-full flex-col items-center justify-center gap-6 opacity-0"
    >
      <div class="flex w-full flex-col gap-2">
        <h1
          class="text-text-disabled text-center text-4xl md:text-5xl lg:text-7xl"
        >
          {$t({
            message: "Experience",
            context:
              "Used as an action word inviting the reader to try or feel something, e.g. Experience Real Privacy",
          })}
        </h1>
        <TextFade
          texts={[
            $t`Real Privacy`,
            $t`Full Ownership`,
            $t`Seamless Access`,
            "Internet Identity",
          ]}
          duration={500}
          delayBetween={2000}
          startDelay={2800}
          textClass="text-4xl md:text-5xl lg:text-7xl text-text-primary"
          containerClass="h-[40px] md:h-[48px] lg:h-[72px] w-full flex items-center justify-center"
        />
      </div>
      <p
        class="text-text-tertiary max-w-[600px] text-center text-base text-balance"
      >
        <Trans>
          Internet Identity lets you access apps and services securely, without
          creating passwords, sharing personal data, or giving up control.
        </Trans>
      </p>
    </div>
  </div>
  <div class="overflow-x-auto px-4 pt-4 pb-8 sm:px-8">
    <div
      class="flex min-w-[fit-content] flex-row flex-nowrap justify-center gap-4"
    >
      <LandingCard
        header={$t`Easy access`}
        subheader={$t`Sign in using familiar methods`}
        description={$t`Make sign-up and sign-in simple with Google, Apple, or Microsoft. The login you know with enhanced privacy.`}
      >
        <EasyAccessIllustration
          class={[
            "max-w-[233px]",
            "[&_stop:first-child]:[stop-color:var(--fg-tertiary)]",
            "[&_stop:last-child]:[stop-color:var(--fg-primary)]",
            "dark:[&_stop:first-child]:[stop-color:var(--fg-quaternary)]",
            "dark:[&_stop:last-child]:[stop-color:var(--fg-quaternary_hover)]",
          ]}
        />
      </LandingCard>
      <LandingCard
        header={$t`Password-free`}
        subheader={$t`Discoverable passkeys`}
        description={$t`Forget about remembering complicated usernames and passwords. With passkeys, you simply pick your name to log in — quick, safe, and hassle-free.`}
      >
        <PasswordFreeIllustration
          class={[
            "max-w-[264px]",
            "[&_stop:first-child]:[stop-color:var(--fg-tertiary)]",
            "[&_stop:last-child]:[stop-color:var(--fg-primary)]",
            "dark:[&_stop:first-child]:[stop-color:var(--fg-quaternary)]",
            "dark:[&_stop:last-child]:[stop-color:var(--fg-quaternary_hover)]",
          ]}
        />
      </LandingCard>
      <LandingCard
        header={$t`Full control`}
        subheader={$t`Advanced identity management`}
        description={$t`Manage your identities and stay in control of your apps and websites with your dashboard. Explore Pro Features to further customize and secure your experience.`}
      >
        <FullControlIllustration
          class={[
            "max-w-[170px]",
            "[&_stop:first-child]:[stop-color:var(--fg-tertiary)]",
            "[&_stop:last-child]:[stop-color:var(--fg-primary)]",
            "dark:[&_stop:first-child]:[stop-color:var(--fg-quaternary)]",
            "dark:[&_stop:last-child]:[stop-color:var(--fg-quaternary_hover)]",
          ]}
        />
      </LandingCard>
    </div>
  </div>
  <div
    class="mx-auto flex max-w-7xl flex-col gap-10 px-4 py-24 md:px-16 lg:flex-row"
  >
    <div class="flex flex-1 flex-col gap-5">
      <div class="flex flex-col gap-3">
        <p
          class="text-text-tertiary text-center text-xs md:text-left md:text-sm"
        >
          {$t`Understanding Internet Identity`}
        </p>
        <p
          class="text-text-primary text-center text-3xl font-semibold md:text-left md:text-4xl"
        >
          {$t`FAQs`}
        </p>
      </div>
      <p class="text-text-tertiary text-center text-sm md:text-left md:text-lg">
        <Trans>
          Everything you need to know about Internet Identity, your private,
          secure, and easy way to log in to apps on the Internet Computer.
        </Trans>
      </p>
    </div>
    <div
      class={[
        // Layout, spacing and dividers
        "divide-border-secondary mx-5 flex flex-1 flex-col divide-y-1",
        // Details padding
        "[&_details]:pb-6",
        // Summary layout
        "[&_summary]:-mb-6 [&_summary]:flex [&_summary]:flex-row [&_summary]:items-center [&_summary]:gap-4 [&_summary]:py-10",
        // Summary styling
        "[&_summary]:text-text-primary [&_summary]:cursor-pointer [&_summary]:font-medium [&_summary]:select-none md:[&_summary]:text-xl",
        // Summary icons
        "[&_summary_svg]:text-text-placeholder [&_summary_svg]:ml-auto [&_summary_svg]:size-6 [&_summary_svg]:shrink-0",
        // Toggle summary icons
        "[&_details:not(:open)_summary_svg:first-child]:hidden [&_details:open_summary_svg:last-child]:hidden",
        // Paragraph styling and spacing
        "[&_p]:text-text-secondary [&_p]:mb-4 [&_p]:text-base",
        // Paragraph + list spacing
        "[&_ul]:mb-4",
        // List styling and spacing
        "[&_li]:text-text-secondary [&_li]:text-base [&_li_+_li]:mt-4",
      ]}
    >
      <details>
        <summary>
          {$t`What is Internet Identity?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Internet Identity is an authentication system for applications
            running on the
            <a
              href={INTERNET_COMPUTER_URL}
              class="text-text-primary underline"
              target="_blank">Internet Computer</a
            >. Instead of traditional usernames and passwords, it uses keys
            created via
            <a
              href={FAQ_PASSKEY_URL}
              class="text-text-primary underline"
              target="_blank">passkeys</a
            > or other authentication systems like Google, Microsoft or Apple.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`Is my Face ID or Fingerprint stored in Internet Identity?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            No. Your face ID or fingerprint is only used to create or unlock
            your passkey. Your biometric data is never shared with Internet
            Identity or with any websites or apps you use.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`How do passkeys work?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Passkeys are a secure and easy way to sign in without using
            passwords. They're stored on your password manager (Google Password
            Manager, iCloud Keychain, 1Password, etc) and can be protected by
            your fingerprint, face, or device code.
          </Trans>
        </p>
        <p>
          <Trans>
            When you create a passkey, your device makes two matching keys. One
            stays safely on your device, and the other goes to the app or
            service. They work together to confirm it's you — without ever
            sharing your personal info or a password.
          </Trans>
        </p>
        <p>
          <Trans>
            Passkeys are phishing-resistant, hard to steal, and can sync across
            your devices through providers like Google, Apple, or Microsoft.
            <a
              href={FAQ_PASSKEY_URL}
              class="text-text-primary underline"
              target="_blank">Learn more.</a
            >
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`What makes Internet Identity secure & easy to use?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Internet Identity is as secure as the authentication methods it uses
            — like Google, Apple, Microsoft, or Passkeys. That means it's built
            on state-of-the-art security. When you choose Passkeys, access is
            protected by your device's unlock method (Face ID or fingerprint),
            making it as simple as unlocking your phone.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`What makes Internet Identity privacy-preserving?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Internet Identity never shares your personal data, like your email,
            with apps or websites. Instead, it creates a unique pseudonym for
            you in each app. Every app gets a different one, so they can’t track
            you across services. All of this happens automatically in the
            background — you don't need to manage anything.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`How does Internet Identity compare to other Web3 authentication tools?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Unlike many Web3 auth systems that require manual signing for every
            action, Internet Identity creates authenticated sessions per
            application. This delivers both security and convenience—no repeated
            signing—while still giving each session its own identity.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`How do Google, Microsoft and Apple integrations work with Internet Identity?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            The integration with Google, Microsoft and Apple uses a standard
            authentication protocol called OpenID. This is the same protocol
            that other applications use to log in with Google.
          </Trans>
        </p>
        <p>
          <Trans>
            Internet Identity then bridges the authentication with those
            providers with the Internet Computer. By authenticating with one of
            those services, Internet Identity can ensure that you are who you
            are, and then create another authentication for the applications
            that run on Internet Computer.
          </Trans>
        </p>
        <p>
          <Trans>
            No data is shared with Google, Microsoft or Apple on which
            applications you log in with Internet Identity.
          </Trans>
        </p>
        <p>
          <Trans>
            For example, user A logs into Internet Identity with their Google
            account “A@gmail.com” and then uses Internet Identity to
            authenticate in application Z. Google will never know that the user
            is authenticated in application Z, only with Internet Identity.
            Moreover, application Z will also never know that user A used Google
            to authenticate their Internet Identity user.
          </Trans>
        </p>
      </details>
      <details>
        <summary>
          {$t`What's new in Internet Identity 2.0?`}
          <MinusCircleIcon />
          <PlusCircleIcon />
        </summary>
        <p>
          <Trans>
            Internet Identity 2.0 brings a host of improvements designed to
            enhance your experience:
          </Trans>
        </p>
        <ul>
          <li>
            <Trans>
              <span class="text-text-primary font-semibold">
                Completely Redesigned Interface:
              </span>
              We've given Internet Identity a fresh, modern look and feel. The new
              design is intuitive and easier to navigate, ensuring a smoother user
              journey.
            </Trans>
          </li>
          <li>
            <Trans>
              <span class="text-text-primary font-semibold">
                No More Identity Numbers:
              </span>
              As explained above, discoverable passkeys mean you no longer need to
              remember or store an identity number. Logging in is now simpler and
              more streamlined.
            </Trans>
          </li>
          <li>
            <Trans>
              <span class="text-text-primary font-semibold">
                Seamless Passkey Integration:
              </span>
              We continue to embrace passkeys as the future of secure authentication.
              Internet Identity 2.0 leverages the latest passkey standards for enhanced
              security and ease of use.
            </Trans>
          </li>
          <li>
            <Trans>
              <span class="text-text-primary font-semibold">
                Google, Apple and Microsoft Integrations:
              </span>
              While passkeys are becoming more mature, they are not yet mainstream
              for everyone. We've observed a 50% drop-off rate in our registration
              flow, indicating a need for alternative authentication methods. To
              make Internet Identity accessible to an even wider audience, we've
              integrated with Google, Apple and Microsoft as alternative authentication
              options.
            </Trans>
          </li>
        </ul>
      </details>
    </div>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

<style>
  .fade-in {
    animation: fadeIn 0.6s ease-in 0.2s forwards;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }

  /* Animate details height in supported browsers */
  @supports (interpolate-size: allow-keywords) {
    details {
      /*noinspection CssInvalidPseudoSelector*/

      &::details-content {
        overflow: clip;
        display: block;
        transition:
          content-visibility 0.2s,
          height 0.2s ease;
        transition-behavior: allow-discrete;
        /*noinspection CssUnknownProperty*/
        interpolate-size: allow-keywords;
        height: 0;
      }

      /*noinspection CssInvalidPseudoSelector*/

      &[open]::details-content {
        height: auto;
      }
    }
  }
</style>
