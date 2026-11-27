<!--
  What the brand side says: one statement at a time (with a pager to the
  others), a quote, or three principles. The copy is a draft to judge the
  layouts by; every claim in it holds for Internet Identity today, but it
  wants a writer's pass before it goes anywhere.

  `tinted`: it stands on an identity's colours, so it takes their
  foreground (the colour it's set in) and steps down by opacity, not by the
  theme's greys.
-->
<script lang="ts" module>
  export const STATEMENTS = [
    {
      title: "Your identity.\nNot a platform's.",
      body: "No company holds the keys to it. They stay in your passkeys, on your own devices.",
    },
    {
      title: "Every app sees\na different you.",
      body: "Each app gets its own identifier for you, so no one can follow you from one app to the next.",
    },
    {
      title: "Nothing here\nto leak.",
      body: "You sign in with your face, your fingerprint or your screen lock. There is no password on a server to steal.",
    },
    {
      title: "Run by no\ncompany.",
      body: "Internet Identity is open source, runs on the Internet Computer, and is governed by its community.",
    },
  ];

  export const PRINCIPLES = [
    {
      title: "Yours",
      body: "Your keys stay in your passkeys, on your devices. No company can take them or lock you out.",
    },
    {
      title: "Private",
      body: "Each app sees a different identity, so what you do can't be joined up across apps.",
    },
    {
      title: "Open",
      body: "Open source, on the Internet Computer, governed by the people who use it.",
    },
  ];
</script>

<script lang="ts">
  import type { BrandId } from "../playground.svelte";

  interface Props {
    brand: BrandId;
    tinted?: boolean;
    /** `hero`: as large as the page allows, for a layout that is the brand. */
    scale?: "panel" | "hero" | "band";
  }

  const { brand, tinted = false, scale = "panel" }: Props = $props();

  let index = $state(0);
  const statement = $derived(STATEMENTS[index]);

  const TITLE = $derived(
    {
      panel:
        "text-[2.5rem] leading-[1.04] lg:text-[3.25rem] xl:text-[4rem] tracking-[-0.035em]",
      hero: "text-[3rem] leading-[1.02] md:text-[4.5rem] xl:text-[5.75rem] tracking-[-0.04em]",
      band: "text-[1.75rem] leading-[1.08] tracking-[-0.03em]",
    }[scale],
  );
  const BODY = $derived(
    {
      panel: "text-base lg:text-lg max-w-[26rem]",
      hero: "text-lg max-w-[30rem]",
      band: "text-sm max-w-[22rem]",
    }[scale],
  );
  const primary = $derived(tinted ? "" : "text-text-primary");
  const quiet = $derived(tinted ? "opacity-70" : "text-text-tertiary");
</script>

{#if brand === "statement"}
  <div class="flex flex-col gap-5">
    {#key index}
      <h2
        class={[
          "font-semibold text-balance whitespace-pre-line",
          TITLE,
          primary,
        ]}
      >
        {statement.title}
      </h2>
      <p class={["text-pretty", BODY, quiet]}>{statement.body}</p>
    {/key}
    {#if scale !== "band"}
      <div class="mt-2 flex gap-1.5" role="tablist" aria-label="Statements">
        {#each STATEMENTS as one, at (one.title)}
          <button
            type="button"
            role="tab"
            aria-selected={at === index}
            aria-label={`Statement ${at + 1}`}
            onclick={() => (index = at)}
            class="group grid h-6 place-items-center outline-none"
          >
            <span
              class={[
                "group-focus-visible:ring-focus-ring block h-1 rounded-full transition-[width,opacity] duration-300 ease-(--ii-ease-out) group-focus-visible:ring-2",
                at === index ? "w-6 opacity-100" : "w-3 opacity-30",
                tinted ? "bg-current" : "bg-fg-primary",
              ]}
            ></span>
          </button>
        {/each}
      </div>
    {/if}
  </div>
{:else if brand === "quote"}
  <figure class="flex flex-col gap-6">
    <blockquote
      class={[
        "font-semibold text-balance",
        scale === "band"
          ? "text-xl leading-snug tracking-[-0.02em]"
          : scale === "hero"
            ? "text-[2rem] leading-[1.15] tracking-[-0.03em] md:text-[3rem] xl:text-[3.5rem]"
            : "text-[1.75rem] leading-[1.18] tracking-[-0.025em] lg:text-[2.25rem] xl:text-[2.625rem]",
        primary,
      ]}
    >
      <span aria-hidden="true" class="-ms-[0.45em]">“</span>No one shall be
      subjected to arbitrary interference with his privacy, family, home or
      correspondence.”
    </blockquote>
    <figcaption class={["flex flex-col gap-0.5 text-sm", quiet]}>
      <span
        class={["font-medium", tinted ? "opacity-100" : "text-text-secondary"]}
      >
        Universal Declaration of Human Rights
      </span>
      <span>Article 12, 1948</span>
    </figcaption>
  </figure>
{:else}
  <ol
    class={[
      "flex",
      scale === "band" ? "flex-col gap-2" : "flex-col gap-7 xl:gap-9",
    ]}
  >
    {#each PRINCIPLES as one, at (one.title)}
      <li
        class={[
          "grid items-baseline",
          scale === "band"
            ? "grid-cols-[2rem_1fr] gap-x-2"
            : "grid-cols-[3rem_1fr] gap-x-4 xl:grid-cols-[4rem_1fr]",
        ]}
      >
        <span class={["text-sm font-medium tabular-nums", quiet]}>
          0{at + 1}
        </span>
        <span class="flex flex-col gap-1.5">
          <span
            class={[
              "font-semibold",
              scale === "band"
                ? "text-base tracking-[-0.01em]"
                : "text-[2rem] leading-[1.05] tracking-[-0.03em] xl:text-[2.75rem]",
              primary,
            ]}
          >
            {one.title}
          </span>
          {#if scale !== "band"}
            <span class={["max-w-[24rem] text-base text-pretty", quiet]}>
              {one.body}
            </span>
          {/if}
        </span>
      </li>
    {/each}
  </ol>
{/if}
