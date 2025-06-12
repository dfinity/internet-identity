<script lang="ts">
  import { onDestroy, onMount } from "svelte";

  let {
    content,
    speed = 50,
    pauseOnHover = true,
    href,
    fixed,
    class: classes,
  }: {
    content: string;
    speed?: number;
    pauseOnHover: boolean;
    href?: string;
    fixed?: boolean;
    class?: string;
  } = $props();

  let contentWidth = $state<number>(1);
  let containerWidth = $state<number>(1);
  let isHovered = $state(false);
  let copies = $derived(Math.ceil(containerWidth / contentWidth) * 2);
  let observer: MutationObserver | null = null;

  function applyClasses() {
    let logo = document.querySelector(".c-landingPage__logo");
    if (logo) {
      logo.classList.add("c-landingPage__logo__marqueespaced");
    }

    let lContainer = document.querySelector(".l-container");
    if (lContainer) {
      lContainer.classList.add("l-container__marqueespaced");
    }
  }

  onMount(() => {
    applyClasses();

    // This observer is necessary because the lContainer does not exist when the page loads
    observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        if (mutation.type === "childList") {
          applyClasses();
        }
      }
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    });
  });

  onDestroy(() => {
    if (observer) {
      observer.disconnect();
    }

    let logo = document.querySelector(".c-landingPage__logo");
    if (logo) {
      logo.classList.remove("c-landingPage__logo__marqueespaced");
    }

    let lContainer = document.querySelector(".l-container");
    if (lContainer) {
      lContainer.classList.remove("l-container__marqueespaced");
    }
  });
</script>

<div
  class={["marquee", classes, fixed && "fixed"]}
  bind:clientWidth={containerWidth}
  onmouseenter={() => (isHovered = true)}
  onmouseleave={() => (isHovered = false)}
  role="marquee"
>
  {#if href}
    <a
      class="marquee-link"
      target="_blank"
      style:animation-duration={`${contentWidth / speed}s`}
      class:paused={isHovered && pauseOnHover}
      {href}
    >
      <span bind:offsetWidth={contentWidth}>{content + " "}</span>|
      {#each Array(copies) as _}
        <span aria-hidden="true">{content + " "}</span>
      {/each}
    </a>
  {:else}
    <div
      class="marquee-scroll"
      style:animation-duration={`${contentWidth / speed}s`}
      class:paused={isHovered && pauseOnHover}
    >
      <span bind:offsetWidth={contentWidth}>{content + " "}</span>
      {#each Array(copies) as _}
        <span>{content + " "}</span>
      {/each}
    </div>
  {/if}
</div>

<style>
  .marquee {
    position: relative;
    width: 100%;
    overflow: hidden;
    white-space: nowrap;
    padding-top: 1rem;
    padding-bottom: 1rem;
    background-color: var(--rc-warning-orange);
    pointer-events: none;
  }

  .marquee-scroll {
    display: inline-block;
    animation: scroll linear infinite;
    user-select: none;
  }

  .marquee-link {
    display: inline-block;
    animation: scroll linear infinite;
    user-select: none;
    pointer-events: all;
    cursor: pointer;
    text-decoration-thickness: 2px;
    text-underline-offset: 4px;
    text-decoration-color: var(--vc-snow);
    color: var(--vc-snow);
  }

  .marquee-scroll.paused {
    animation-play-state: paused;
  }
  .marquee-link.paused {
    animation-play-state: paused;
  }

  .fixed {
    position: fixed;
    top: 0;
    z-index: 10;
  }

  @keyframes scroll {
    0% {
      transform: translateX(0);
    }
    100% {
      transform: translateX(-50%);
    }
  }
</style>
