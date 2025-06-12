<script lang="ts">
  let {
    content,
    speed = 50,
    pauseOnHover = true,
    href,
    fixed, // adding this because using components in the old flow is headache
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

  // TODO show/hide temporarily show only between monday and thursday 10am
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
    background-color: var(--rc-warning);
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
