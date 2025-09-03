<!-- Due to the linear design of the stroke, colors must be passed through from the parent in order to manage both light and dark mode changes without simply duplicating the image -->

<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement> & {
    colors: { start: string; end: string };
  };

  const { class: className, colors, ...props }: Props = $props();
</script>

<svg
  {...props}
  viewBox="0 0 234 206"
  fill="none"
  class={className}
  xmlns="http://www.w3.org/2000/svg"
>
  <g clip-path="url(#clip0_14810_25746)">
    <path
      d="M84.9329 162.133H54.6728V141.972H107.993C111.398 164.572 101.441 187.27 81.3653 198.488C39.5265 221.836 -9.05845 183.888 3.2013 137.648C11.5691 106.04 46.9537 89.9765 76.8246 102.951C79.0625 103.927 91.0628 110.723 90.706 112.706L75.9814 127.372C51.1376 107.471 15.9151 129.648 23.9585 160.8C32.002 191.952 76.5003 191.562 84.9978 162.101L84.9329 162.133Z"
      stroke="url(#paint0_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M164.07 0.390137H122.555V42.4681H164.07V0.390137Z"
      stroke="url(#paint1_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M117.82 46.6304H76.3057V88.7084H117.82V46.6304Z"
      stroke="url(#paint2_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M164.07 46.6304H122.555V88.7084H164.07V46.6304Z"
      stroke="url(#paint3_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M117.82 0.390137H76.3057V42.4681H117.82V0.390137Z"
      stroke="url(#paint4_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M230.169 121.812C210.255 131.014 213.174 159.402 233.315 166.426C231.92 170.458 230.266 174.393 228.19 178.1C225.304 183.205 217.877 193.091 212.752 195.692C203.314 200.537 197.736 193.188 188.752 192.83C178.178 192.408 171.238 201.35 161.118 194.879C133.031 176.897 127.031 106.561 169.843 111.406C175.973 112.089 182.232 116.609 187.487 116.251C191.054 115.991 193.844 113.357 197.119 112.186C208.601 107.992 223.228 111.634 230.169 121.812Z"
      stroke="url(#paint5_linear_14810_25746)"
      stroke-miterlimit="10"
    />
    <path
      d="M208.957 82.5627C209.865 96.903 199.324 109.552 184.632 109.357C182.913 95.1145 194.557 81.9448 208.957 82.5627Z"
      stroke="url(#paint6_linear_14810_25746)"
      stroke-miterlimit="10"
    />
  </g>
  <defs>
    <linearGradient
      id="paint0_linear_14810_25746"
      x1="54.9618"
      y1="98.4397"
      x2="54.9618"
      y2="205.579"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint1_linear_14810_25746"
      x1="143.312"
      y1="0.390137"
      x2="143.312"
      y2="42.4681"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint2_linear_14810_25746"
      x1="97.0629"
      y1="46.6304"
      x2="97.0629"
      y2="88.7084"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint3_linear_14810_25746"
      x1="143.312"
      y1="46.6304"
      x2="143.312"
      y2="88.7084"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint4_linear_14810_25746"
      x1="97.0629"
      y1="0.390137"
      x2="97.0629"
      y2="42.4681"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint5_linear_14810_25746"
      x1="186.099"
      y1="110.477"
      x2="186.099"
      y2="197.339"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint6_linear_14810_25746"
      x1="196.737"
      y1="82.5417"
      x2="196.737"
      y2="109.36"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <clipPath id="clip0_14810_25746">
      <rect
        width="233"
        height="206"
        fill="white"
        transform="translate(0.833496)"
      />
    </clipPath>
  </defs>
</svg>
