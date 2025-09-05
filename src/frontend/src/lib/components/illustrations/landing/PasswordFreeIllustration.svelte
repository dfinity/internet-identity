<!-- Gradients for SVG strokes must be defined with <linearGradient> inside the SVG. 
     We pass colours down from the parent so they update in light and dark mode 
     without duplicating the SVG definition. 
     Docs: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/linearGradient 
           https://developer.mozilla.org/en-US/docs/Web/SVG/Element/stop -->

<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement> & {
    colors: { start: string; end: string };
  };

  const { class: className, colors, ...props }: Props = $props();
</script>

<svg
  {...props}
  viewBox="0 0 264 190"
  fill="none"
  class={className}
  xmlns="http://www.w3.org/2000/svg"
>
  <path
    d="M55.4193 69.2048H54.5108C46.8786 69.2048 43.0625 69.2048 40.1474 70.6902C37.5832 71.9967 35.4984 74.0814 34.1919 76.6457C32.7065 79.5608 32.7065 83.3769 32.7065 91.009V91.9176M55.4193 150.971H54.5108C46.8786 150.971 43.0625 150.971 40.1474 149.485C37.5832 148.179 35.4984 146.094 34.1919 143.53C32.7065 140.615 32.7065 136.799 32.7065 129.166V128.258M114.472 91.9176V91.009C114.472 83.3769 114.472 79.5608 112.987 76.6457C111.68 74.0814 109.596 71.9967 107.032 70.6902C104.116 69.2048 100.3 69.2048 92.6681 69.2048H91.7596M114.472 128.258V129.166C114.472 136.799 114.472 140.615 112.987 143.53C111.68 146.094 109.596 148.179 107.032 149.485C104.116 150.971 100.3 150.971 92.6681 150.971H91.7596M53.148 91.9176V98.7314M94.0309 91.9176V98.7314M69.0469 112.814C72.6809 112.814 75.8607 109.634 75.8607 106V91.9176M88.1265 124.624C79.9499 132.8 66.7765 132.8 58.5999 124.624"
    stroke="url(#paint0_linear_14810_25760)"
    stroke-linecap="round"
    stroke-linejoin="round"
  />
  <rect
    x="0.5"
    y="36.9983"
    width="148.223"
    height="148.223"
    rx="15.5"
    stroke="url(#paint1_linear_14810_25760)"
  />
  <path
    d="M179.591 143.406C179.591 143.406 175.255 134.182 178.987 122.862"
    stroke="url(#paint2_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M180.146 119.913C180.146 119.913 187.315 102.064 208.901 102.064"
    stroke="url(#paint3_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M238.788 133.465C238.788 133.465 240.015 107.407 212.32 102.333"
    stroke="url(#paint4_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M182.731 150.483C183.255 148.275 183.834 144.882 183.598 140.753C183.411 137.472 182.872 136.64 182.715 133.795C182.635 132.316 182.409 127.159 184.92 121.688C187.597 115.854 192.058 112.567 194.364 111.112"
    stroke="url(#paint5_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M197.443 109.446C199.114 108.675 207.01 105.245 216.377 108.351C225.119 111.249 229.316 117.902 230.488 119.949C232.478 123.421 233.14 127.652 233.796 131.04C234.311 133.707 234.563 135.969 234.692 137.578"
    stroke="url(#paint6_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M234.799 148.045C234.826 145.665 234.853 143.285 234.884 140.905"
    stroke="url(#paint7_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M186.239 154.663C187.248 152.452 188.517 148.995 188.741 144.664C188.968 140.337 187.947 139.066 187.858 134.587C187.784 130.925 187.68 125.626 190.945 120.742C195.008 114.666 201.763 113.003 203.588 112.625C208.546 111.591 212.639 112.616 214.589 113.245"
    stroke="url(#paint8_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M217.518 114.469C219.547 115.564 222.807 117.69 225.269 121.47C227.222 124.47 227.747 127.089 228.725 131.98C229.215 134.438 230.022 139.302 229.829 145.607C229.7 149.763 229.175 153.241 228.673 155.778"
    stroke="url(#paint9_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M190.185 157.995C190.725 156.806 191.261 155.478 191.758 154.018C192.236 152.611 192.607 151.284 192.896 150.056"
    stroke="url(#paint10_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M193.64 146.778C193.818 145.538 194.008 143.642 193.889 141.331C193.763 138.93 193.404 137.959 193.153 135.534C192.886 132.936 192.665 130.798 193.153 128.502C194.211 123.509 198.022 120.527 198.592 120.095C202.628 117.028 206.933 117.031 208.147 117.049C211.768 117.107 214.377 118.416 215.202 118.861C218.235 120.5 219.909 122.759 220.636 123.884"
    stroke="url(#paint11_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M204.568 163.367C204.927 162.426 205.289 161.486 205.648 160.542C206.013 159.586 206.374 158.634 206.736 157.681"
    stroke="url(#paint12_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M210.002 163.388C210.906 161.241 211.82 158.685 212.556 155.758C213.335 152.661 213.746 149.839 213.951 147.423"
    stroke="url(#paint13_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M215.782 162.51C216.803 159.773 217.91 156.196 218.655 151.916C219.584 146.578 219.526 142.559 219.464 139.232C219.311 131.266 218.078 128.901 217.554 127.997C216.898 126.863 215.583 124.658 212.851 123.285C210.296 122.003 207.886 122.139 207.19 122.197C205.602 122.333 204.363 122.804 203.587 123.167"
    stroke="url(#paint14_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M222.193 159.891C223.447 155.626 224.717 149.605 224.68 142.347C224.649 136.095 223.665 130.824 222.616 126.886"
    stroke="url(#paint15_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M201.079 124.88C199.877 125.957 198.305 127.714 197.783 130.172C197.222 132.813 198.252 134.503 198.813 138.002C199.335 141.269 199.068 143.715 198.74 146.7C198.4 149.791 197.446 154.718 194.417 160.455"
    stroke="url(#paint16_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M199.267 162.224C200.579 159.55 202.042 155.927 203.002 151.481C203.643 148.512 204.474 144.665 204.032 140.174C203.863 138.459 203.787 136.738 203.59 135.026C203.232 131.902 202.962 131.043 203.517 129.879C204.21 128.421 205.78 127.351 207.485 127.124C209.748 126.824 211.82 128.073 212.703 129.589C212.844 129.831 212.998 130.154 213.366 132.199C213.688 133.995 213.859 134.948 214.028 136.765C214.194 138.55 214.341 140.972 214.295 143.93"
    stroke="url(#paint17_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <path
    d="M207.558 154.798C208.291 151.456 208.913 147.265 208.953 142.42C208.99 138.11 208.558 134.33 207.997 131.23"
    stroke="url(#paint18_linear_14810_25760)"
    stroke-miterlimit="10"
    stroke-linecap="round"
  />
  <rect
    x="152.583"
    y="77.8359"
    width="110.917"
    height="110.917"
    rx="11.5"
    stroke="url(#paint19_linear_14810_25760)"
  />
  <rect
    x="152.583"
    y="1.24707"
    width="73.6113"
    height="73.6113"
    rx="7.5"
    stroke="url(#paint20_linear_14810_25760)"
  />
  <path
    d="M184.846 24.1826L175.519 33.509"
    stroke="url(#paint21_linear_14810_25760)"
    stroke-linecap="round"
  />
  <path
    d="M203.259 42.5964L193.933 51.9228"
    stroke="url(#paint22_linear_14810_25760)"
    stroke-linecap="round"
  />
  <path
    d="M183.171 38.0527H176.953"
    stroke="url(#paint23_linear_14810_25760)"
    stroke-linecap="round"
  />
  <path
    d="M201.824 38.0527H195.607"
    stroke="url(#paint24_linear_14810_25760)"
    stroke-linecap="round"
  />
  <circle
    cx="170.975"
    cy="38.0528"
    r="2.84794"
    stroke="url(#paint25_linear_14810_25760)"
  />
  <circle
    cx="189.388"
    cy="38.0528"
    r="2.84794"
    stroke="url(#paint26_linear_14810_25760)"
  />
  <circle
    cx="207.802"
    cy="38.0528"
    r="2.84794"
    stroke="url(#paint27_linear_14810_25760)"
  />
  <circle
    cx="170.975"
    cy="56.4664"
    r="2.84794"
    stroke="url(#paint28_linear_14810_25760)"
  />
  <circle
    cx="189.388"
    cy="56.4664"
    r="2.84794"
    stroke="url(#paint29_linear_14810_25760)"
  />
  <circle
    cx="207.802"
    cy="56.4664"
    r="2.84794"
    stroke="url(#paint30_linear_14810_25760)"
  />
  <circle
    cx="170.975"
    cy="19.6392"
    r="2.84794"
    stroke="url(#paint31_linear_14810_25760)"
  />
  <circle
    cx="189.388"
    cy="19.6392"
    r="2.84794"
    stroke="url(#paint32_linear_14810_25760)"
  />
  <circle
    cx="207.802"
    cy="19.6392"
    r="2.84794"
    stroke="url(#paint33_linear_14810_25760)"
  />
  <defs>
    <linearGradient
      id="paint0_linear_14810_25760"
      x1="73.5894"
      y1="69.2048"
      x2="73.5894"
      y2="150.971"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint1_linear_14810_25760"
      x1="74.6113"
      y1="36.4983"
      x2="74.6113"
      y2="185.721"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint2_linear_14810_25760"
      x1="178.531"
      y1="122.862"
      x2="178.531"
      y2="143.406"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint3_linear_14810_25760"
      x1="194.524"
      y1="102.064"
      x2="194.524"
      y2="119.913"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint4_linear_14810_25760"
      x1="225.558"
      y1="102.333"
      x2="225.558"
      y2="133.465"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint5_linear_14810_25760"
      x1="188.518"
      y1="111.112"
      x2="188.518"
      y2="150.483"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint6_linear_14810_25760"
      x1="216.067"
      y1="107.072"
      x2="216.067"
      y2="137.578"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint7_linear_14810_25760"
      x1="234.841"
      y1="140.905"
      x2="234.841"
      y2="148.045"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint8_linear_14810_25760"
      x1="200.414"
      y1="112.184"
      x2="200.414"
      y2="154.663"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint9_linear_14810_25760"
      x1="223.688"
      y1="114.469"
      x2="223.688"
      y2="155.778"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint10_linear_14810_25760"
      x1="191.54"
      y1="150.056"
      x2="191.54"
      y2="157.995"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint11_linear_14810_25760"
      x1="206.75"
      y1="117.047"
      x2="206.75"
      y2="146.778"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint12_linear_14810_25760"
      x1="205.652"
      y1="157.681"
      x2="205.652"
      y2="163.367"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint13_linear_14810_25760"
      x1="211.977"
      y1="147.423"
      x2="211.977"
      y2="163.388"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint14_linear_14810_25760"
      x1="211.541"
      y1="122.162"
      x2="211.541"
      y2="162.51"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint15_linear_14810_25760"
      x1="223.437"
      y1="126.886"
      x2="223.437"
      y2="159.891"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint16_linear_14810_25760"
      x1="197.748"
      y1="124.88"
      x2="197.748"
      y2="160.455"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint17_linear_14810_25760"
      x1="206.785"
      y1="127.079"
      x2="206.785"
      y2="162.224"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint18_linear_14810_25760"
      x1="208.257"
      y1="131.23"
      x2="208.257"
      y2="154.798"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint19_linear_14810_25760"
      x1="208.041"
      y1="77.3359"
      x2="208.041"
      y2="189.253"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint20_linear_14810_25760"
      x1="189.388"
      y1="0.74707"
      x2="189.388"
      y2="75.3584"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint21_linear_14810_25760"
      x1="185.199"
      y1="24.5362"
      x2="175.873"
      y2="33.8626"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint22_linear_14810_25760"
      x1="203.613"
      y1="42.95"
      x2="194.286"
      y2="52.2764"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint23_linear_14810_25760"
      x1="183.171"
      y1="38.5527"
      x2="176.953"
      y2="38.5527"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint24_linear_14810_25760"
      x1="201.824"
      y1="38.5527"
      x2="195.607"
      y2="38.5527"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint25_linear_14810_25760"
      x1="170.975"
      y1="34.7048"
      x2="170.975"
      y2="41.4007"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint26_linear_14810_25760"
      x1="189.388"
      y1="34.7048"
      x2="189.388"
      y2="41.4007"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint27_linear_14810_25760"
      x1="207.802"
      y1="34.7048"
      x2="207.802"
      y2="41.4007"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint28_linear_14810_25760"
      x1="170.975"
      y1="53.1184"
      x2="170.975"
      y2="59.8143"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint29_linear_14810_25760"
      x1="189.388"
      y1="53.1184"
      x2="189.388"
      y2="59.8143"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint30_linear_14810_25760"
      x1="207.802"
      y1="53.1184"
      x2="207.802"
      y2="59.8143"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint31_linear_14810_25760"
      x1="170.975"
      y1="16.2913"
      x2="170.975"
      y2="22.9871"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint32_linear_14810_25760"
      x1="189.388"
      y1="16.2913"
      x2="189.388"
      y2="22.9871"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
    <linearGradient
      id="paint33_linear_14810_25760"
      x1="207.802"
      y1="16.2913"
      x2="207.802"
      y2="22.9871"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0" stop-color={colors.start} />
      <stop offset="0.94" stop-color={colors.end} />
    </linearGradient>
  </defs>
</svg>
