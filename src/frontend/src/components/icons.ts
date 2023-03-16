import { html } from "lit-html";

export const logoutIcon = html`
  <svg
    id="logoutIcon"
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M20.8195 17.7516H19.5586C19.4602 17.7516 19.3781 17.8336 19.3781 17.932V19.3805H4.61953V4.61953H19.3805V6.06797C19.3805 6.16641 19.4625 6.24844 19.5609 6.24844H20.8219C20.9203 6.24844 21.0023 6.16875 21.0023 6.06797V3.71953C21.0023 3.32109 20.6813 3 20.2828 3H3.71953C3.32109 3 3 3.32109 3 3.71953V20.2805C3 20.6789 3.32109 21 3.71953 21H20.2805C20.6789 21 21 20.6789 21 20.2805V17.932C21 17.8312 20.918 17.7516 20.8195 17.7516ZM21.2555 11.8523L17.9297 9.22734C17.8055 9.12891 17.625 9.21797 17.625 9.375V11.1562H10.2656C10.1625 11.1562 10.0781 11.2406 10.0781 11.3438V12.6562C10.0781 12.7594 10.1625 12.8438 10.2656 12.8438H17.625V14.625C17.625 14.782 17.8078 14.8711 17.9297 14.7727L21.2555 12.1477C21.2779 12.1301 21.296 12.1077 21.3085 12.0821C21.3209 12.0565 21.3274 12.0285 21.3274 12C21.3274 11.9715 21.3209 11.9435 21.3085 11.9179C21.296 11.8923 21.2779 11.8699 21.2555 11.8523Z"
      fill="#292A2E"
    />
  </svg>
`;

export const checkmarkIcon = html`
  <svg
    id="checkmarkIcon"
    xmlns="http://www.w3.org/2000/svg"
    width="17"
    height="16"
    viewBox="0 0 17 16"
    fill="none"
  >
    <path
      d="M14.7493 2.96875H13.6571C13.504 2.96875 13.3587 3.03906 13.2649 3.15937L6.82271 11.3203L3.73365 7.40625C3.68692 7.34692 3.62736 7.29895 3.55943 7.26593C3.49151 7.23292 3.41699 7.21572 3.34146 7.21562H2.24928C2.14459 7.21562 2.08678 7.33594 2.15084 7.41719L6.43053 12.8391C6.63053 13.0922 7.0149 13.0922 7.21646 12.8391L14.8477 3.16875C14.9118 3.08906 14.854 2.96875 14.7493 2.96875V2.96875Z"
      fill="currentColor"
    />
  </svg>
`;

export const icLogo = html`
  <div>
    <svg
      id="icLogo"
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 233 111"
    >
      <defs>
        <linearGradient
          id="grad-o-y"
          x1="145.304"
          x2="221.385"
          y1="7.174"
          y2="85.958"
          gradientUnits="userSpaceOnUse"
        >
          <stop offset=".21" stop-color="#F15A24" />
          <stop offset=".684" stop-color="#FBB03B" />
        </linearGradient>
        <linearGradient
          id="grad-p-p"
          x1="85.087"
          x2="9.006"
          y1="101.622"
          y2="22.838"
          gradientUnits="userSpaceOnUse"
        >
          <stop offset=".21" stop-color="#ED1E79" />
          <stop offset=".893" stop-color="#522785" />
        </linearGradient>
      </defs>
      <g transform="translate(0 2)">
        <path
          fill="url(#grad-o-y)"
          d="M174.433 0c-12.879 0-26.919 6.6-41.758 19.6-7.04 6.159-13.12 12.759-17.679 18.038l.04.04v-.04s7.199 7.84 15.159 16.24c4.28-5.08 10.44-12 17.519-18.24 13.2-11.559 21.799-13.999 26.719-13.999 18.52 0 33.559 14.68 33.559 32.719 0 17.92-15.079 32.599-33.559 32.719-.84 0-1.92-.12-3.28-.4 5.4 2.32 11.2 4 16.72 4 33.918 0 40.558-22.12 40.998-23.72 1-4.04 1.52-8.28 1.52-12.64C230.391 24.4 205.272 0 174.433 0Z"
        />
        <path
          fill="url(#grad-p-p)"
          d="M55.958 108.796c12.88 0 26.919-6.6 41.758-19.6 7.04-6.16 13.12-12.759 17.679-18.039l-.04-.04v.04s-7.199-7.84-15.159-16.24c-4.28 5.08-10.44 12-17.52 18.24-13.199 11.56-21.798 14-26.718 14-18.52-.04-33.559-14.72-33.559-32.76C22.4 36.48 37.48 21.8 55.958 21.68c.84 0 1.92.12 3.28.4-5.4-2.32-11.2-4-16.72-4C8.6 18.08 2 40.2 1.52 41.76A52.8 52.8 0 0 0 0 54.397c0 29.999 25.119 54.398 55.958 54.398Z"
        />
        <path
          fill="#29ABE2"
          d="M187.793 90.197c-17.36-.44-35.399-14.12-39.079-17.52-9.519-8.8-31.479-32.599-33.198-34.479C99.436 20.16 77.637 0 55.958 0h-.08C29.558.12 7.44 17.96 1.52 41.758c.44-1.56 9.12-24.119 40.958-23.319 17.36.44 35.479 14.32 39.199 17.72 9.52 8.8 31.479 32.598 33.199 34.478 16.079 18 37.878 38.159 59.557 38.159h.08c26.319-.12 48.478-17.96 54.358-41.759-.48 1.56-9.2 23.92-41.078 23.16Z"
        />
      <g>
    </svg>
    <h1 class="c-logo__type">Internet Identity</h1>
  </div>
`;

export const closeIcon = html`
  <svg
    class="closeIcon"
    width="16"
    height="16"
    viewBox="0 0 16 16"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M8.8095 8L12.9111 3.11094C12.9798 3.02969 12.922 2.90625 12.8158 2.90625H11.5689C11.4954 2.90625 11.4251 2.93906 11.3767 2.99531L7.99388 7.02813L4.61106 2.99531C4.56419 2.93906 4.49388 2.90625 4.41888 2.90625H3.172C3.06575 2.90625 3.00794 3.02969 3.07669 3.11094L7.17825 8L3.07669 12.8891C3.06129 12.9072 3.05141 12.9293 3.04822 12.9529C3.04503 12.9764 3.04867 13.0004 3.05871 13.022C3.06874 13.0435 3.08475 13.0617 3.10483 13.0745C3.12492 13.0872 3.14823 13.0939 3.172 13.0938H4.41888C4.49231 13.0938 4.56263 13.0609 4.61106 13.0047L7.99388 8.97188L11.3767 13.0047C11.4236 13.0609 11.4939 13.0938 11.5689 13.0938H12.8158C12.922 13.0938 12.9798 12.9703 12.9111 12.8891L8.8095 8Z"
      fill="#262626"
    />
  </svg>
`;

export const warningIcon = html`
  <svg
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      fill-rule="evenodd"
      clip-rule="evenodd"
      d="M8.17317 2.7612C9.38642 2.25866 10.6868 2 12 2C13.3132 2 14.6136 2.25866 15.8268 2.7612C17.0401 3.26375 18.1425 4.00035 19.0711 4.92893C19.9997 5.85752 20.7362 6.95991 21.2388 8.17317C21.7413 9.38642 22 10.6868 22 12C22 14.6522 20.9464 17.1957 19.0711 19.0711C17.1957 20.9464 14.6522 22 12 22C10.6868 22 9.38642 21.7413 8.17317 21.2388C6.95991 20.7362 5.85752 19.9997 4.92893 19.0711C3.05357 17.1957 2 14.6522 2 12C2 9.34784 3.05357 6.8043 4.92893 4.92893C5.85752 4.00035 6.95991 3.26375 8.17317 2.7612ZM12 6.99988C11.7348 6.99988 11.4804 7.10524 11.2929 7.29277C11.1054 7.48031 11 7.73466 11 7.99988V11.9999C11 12.2651 11.1054 12.5194 11.2929 12.707C11.4804 12.8945 11.7348 12.9999 12 12.9999C12.2652 12.9999 12.5196 12.8945 12.7071 12.707C12.8946 12.5194 13 12.2651 13 11.9999V7.99988C13 7.73466 12.8946 7.48031 12.7071 7.29277C12.5196 7.10524 12.2652 6.99988 12 6.99988ZM12 15.9999C12.2652 15.9999 12.5196 15.8945 12.7071 15.707C12.8946 15.5194 13 15.2651 13 14.9999C13 14.7347 12.8946 14.4803 12.7071 14.2928C12.5196 14.1052 12.2652 13.9999 12 13.9999C11.7348 13.9999 11.4804 14.1052 11.2929 14.2928C11.1054 14.4803 11 14.7347 11 14.9999C11 15.2651 11.1054 15.5194 11.2929 15.707C11.4804 15.8945 11.7348 15.9999 12 15.9999Z"
      fill="#F5B400"
    />
  </svg>
`;

export const errorIcon = html`
  <svg
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M22.3988 20.0625L12.6488 3.1875C12.5035 2.93672 12.2527 2.8125 11.9996 2.8125C11.7465 2.8125 11.4934 2.93672 11.3504 3.1875L1.6004 20.0625C1.31212 20.5641 1.67305 21.1875 2.24962 21.1875H21.7496C22.3262 21.1875 22.6871 20.5641 22.3988 20.0625ZM11.2496 9.75C11.2496 9.64687 11.334 9.5625 11.4371 9.5625H12.5621C12.6652 9.5625 12.7496 9.64687 12.7496 9.75V14.0625C12.7496 14.1656 12.6652 14.25 12.5621 14.25H11.4371C11.334 14.25 11.2496 14.1656 11.2496 14.0625V9.75ZM11.9996 18C11.7052 17.994 11.4249 17.8728 11.2188 17.6625C11.0128 17.4522 10.8973 17.1695 10.8973 16.875C10.8973 16.5805 11.0128 16.2978 11.2188 16.0875C11.4249 15.8772 11.7052 15.756 11.9996 15.75C12.294 15.756 12.5743 15.8772 12.7804 16.0875C12.9865 16.2978 13.1019 16.5805 13.1019 16.875C13.1019 17.1695 12.9865 17.4522 12.7804 17.6625C12.5743 17.8728 12.294 17.994 11.9996 18V18Z"
      fill="#292A2E"
      paint-order="stroke"
    />
  </svg>
`;

export const successIcon = html`
  <svg
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M12 1.5C9.9233 1.5 7.89323 2.11581 6.16652 3.26957C4.4398 4.42332 3.09399 6.0632 2.29927 7.98182C1.50455 9.90045 1.29661 12.0116 1.70176 14.0484C2.1069 16.0852 3.10693 17.9562 4.57538 19.4246C6.04383 20.8931 7.91476 21.8931 9.95156 22.2982C11.9884 22.7034 14.0996 22.4955 16.0182 21.7007C17.9368 20.906 19.5767 19.5602 20.7304 17.8335C21.8842 16.1068 22.5 14.0767 22.5 12C22.5 9.21523 21.3938 6.54451 19.4246 4.57538C17.4555 2.60625 14.7848 1.5 12 1.5ZM10.5 16.1925L7.34626 13.0388C7.01696 12.7095 7.01695 12.1755 7.34626 11.8462C7.67556 11.5169 8.20946 11.517 8.53876 11.8463L10.5 13.8075L15.4628 8.84472C15.7915 8.51596 16.3243 8.51513 16.6541 8.84285C16.9854 9.17206 16.9862 9.70776 16.6559 10.038L10.5 16.1925Z"
      fill="#11B10E"
    />
  </svg>
`;

export const securityKeyIcon = html`
  <svg
    width="53"
    height="52"
    viewBox="0 0 53 52"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M39.0938 21.9375V7.3125C39.0938 6.41367 38.3676 5.6875 37.4688 5.6875H15.5312C14.6324 5.6875 13.9062 6.41367 13.9062 7.3125V21.9375C10.5445 21.9375 7.8125 24.5832 7.8125 27.8281V45.9062C7.8125 46.1297 7.99531 46.3125 8.21875 46.3125H11.0625C11.2859 46.3125 11.4688 46.1297 11.4688 45.9062V27.8281C11.4688 26.5941 12.5656 25.5938 13.9113 25.5938H39.0887C40.4344 25.5938 41.5312 26.5941 41.5312 27.8281V45.9062C41.5312 46.1297 41.7141 46.3125 41.9375 46.3125H44.7812C45.0047 46.3125 45.1875 46.1297 45.1875 45.9062V27.8281C45.1875 24.5832 42.4555 21.9375 39.0938 21.9375V21.9375ZM17.5625 21.9375V9.34375H35.4375V21.9375H17.5625ZM23.6562 12.5938H21.2188C20.9953 12.5938 20.8125 12.7766 20.8125 13V15.4375C20.8125 15.6609 20.9953 15.8438 21.2188 15.8438H23.6562C23.8797 15.8438 24.0625 15.6609 24.0625 15.4375V13C24.0625 12.7766 23.8797 12.5938 23.6562 12.5938ZM31.7812 12.5938H29.3438C29.1203 12.5938 28.9375 12.7766 28.9375 13V15.4375C28.9375 15.6609 29.1203 15.8438 29.3438 15.8438H31.7812C32.0047 15.8438 32.1875 15.6609 32.1875 15.4375V13C32.1875 12.7766 32.0047 12.5938 31.7812 12.5938Z"
      fill="#292A2E"
    />
  </svg>
`;
export const seedPhraseIcon = html`
  <svg
    width="53"
    height="52"
    viewBox="0 0 53 52"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M42.75 23.5625H39.2969V12.1875C39.2969 8.59727 36.3871 5.6875 32.7969 5.6875H20.2031C16.6129 5.6875 13.7031 8.59727 13.7031 12.1875V23.5625H10.25C9.35117 23.5625 8.625 24.2887 8.625 25.1875V44.6875C8.625 45.5863 9.35117 46.3125 10.25 46.3125H42.75C43.6488 46.3125 44.375 45.5863 44.375 44.6875V25.1875C44.375 24.2887 43.6488 23.5625 42.75 23.5625ZM17.3594 12.1875C17.3594 10.6184 18.634 9.34375 20.2031 9.34375H32.7969C34.366 9.34375 35.6406 10.6184 35.6406 12.1875V23.5625H17.3594V12.1875ZM40.7188 42.6562H12.2812V27.2188H40.7188V42.6562ZM25.0781 35.5977V38.2891C25.0781 38.5125 25.2609 38.6953 25.4844 38.6953H27.5156C27.7391 38.6953 27.9219 38.5125 27.9219 38.2891V35.5977C28.3411 35.2967 28.6539 34.8705 28.8155 34.3804C28.977 33.8903 28.9789 33.3616 28.8208 32.8704C28.6627 32.3791 28.3529 31.9508 27.9358 31.6468C27.5188 31.3429 27.016 31.1792 26.5 31.1792C25.984 31.1792 25.4812 31.3429 25.0642 31.6468C24.6471 31.9508 24.3373 32.3791 24.1792 32.8704C24.0211 33.3616 24.023 33.8903 24.1845 34.3804C24.3461 34.8705 24.6589 35.2967 25.0781 35.5977Z"
      fill="#292A2E"
    />
  </svg>
`;

export const networkIcon = html`
  <svg
    width="53"
    height="52"
    viewBox="0 0 20 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M19.2812 14.0001H16.8438V10.4376C16.8438 10.3345 16.7594 10.2501 16.6562 10.2501H10.8438V8.00012H13.375C13.5813 8.00012 13.75 7.83137 13.75 7.62512V0.875122C13.75 0.668872 13.5813 0.500122 13.375 0.500122H6.625C6.41875 0.500122 6.25 0.668872 6.25 0.875122V7.62512C6.25 7.83137 6.41875 8.00012 6.625 8.00012H9.15625V10.2501H3.34375C3.24062 10.2501 3.15625 10.3345 3.15625 10.4376V14.0001H0.71875C0.5125 14.0001 0.34375 14.1689 0.34375 14.3751V21.1251C0.34375 21.3314 0.5125 21.5001 0.71875 21.5001H7.46875C7.675 21.5001 7.84375 21.3314 7.84375 21.1251V14.3751C7.84375 14.1689 7.675 14.0001 7.46875 14.0001H4.84375V11.9376H15.1562V14.0001H12.5312C12.325 14.0001 12.1562 14.1689 12.1562 14.3751V21.1251C12.1562 21.3314 12.325 21.5001 12.5312 21.5001H19.2812C19.4875 21.5001 19.6562 21.3314 19.6562 21.1251V14.3751C19.6562 14.1689 19.4875 14.0001 19.2812 14.0001ZM6.0625 15.7814V19.7189H2.125V15.7814H6.0625ZM8.03125 6.21887V2.28137H11.9688V6.21887H8.03125ZM17.875 19.7189H13.9375V15.7814H17.875V19.7189Z"
      fill="#262626"
    />
  </svg>
`;

export const editIcon = html` <svg
  width="20"
  height="20"
  viewBox="0 0 20 20"
  fill="none"
  xmlns="http://www.w3.org/2000/svg"
>
  <path
    d="M4.03984 15.6251C4.08672 15.6251 4.13359 15.6204 4.18047 15.6134L8.12266 14.922C8.16953 14.9126 8.21406 14.8915 8.24688 14.8564L18.182 4.92122C18.2038 4.89953 18.221 4.87378 18.2328 4.84542C18.2445 4.81707 18.2506 4.78668 18.2506 4.75598C18.2506 4.72529 18.2445 4.69489 18.2328 4.66654C18.221 4.63819 18.2038 4.61243 18.182 4.59075L14.2867 0.693091C14.2422 0.64856 14.1836 0.625122 14.1203 0.625122C14.057 0.625122 13.9984 0.64856 13.9539 0.693091L4.01875 10.6282C3.98359 10.6634 3.9625 10.7056 3.95312 10.7525L3.26172 14.6947C3.23892 14.8202 3.24707 14.9494 3.28545 15.0711C3.32384 15.1928 3.39132 15.3033 3.48203 15.3931C3.63672 15.5431 3.83125 15.6251 4.03984 15.6251ZM5.61953 11.5376L14.1203 3.03918L15.8383 4.75715L7.3375 13.2556L5.25391 13.6236L5.61953 11.5376ZM18.625 17.5939H1.375C0.960156 17.5939 0.625 17.929 0.625 18.3439V19.1876C0.625 19.2907 0.709375 19.3751 0.8125 19.3751H19.1875C19.2906 19.3751 19.375 19.2907 19.375 19.1876V18.3439C19.375 17.929 19.0398 17.5939 18.625 17.5939Z"
    fill="#262626"
  />
</svg>`;

export const settingsIcon = html`
  <svg
    width="20"
    height="22"
    viewBox="0 0 20 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M19.6759 13.6634L18.1408 12.3509C18.2134 11.9056 18.2509 11.4509 18.2509 10.9962C18.2509 10.5415 18.2134 10.0869 18.1408 9.64154L19.6759 8.32904C19.7917 8.22992 19.8746 8.09789 19.9135 7.95052C19.9525 7.80315 19.9456 7.64742 19.8939 7.50404L19.8728 7.4431C19.4502 6.26191 18.8173 5.16694 18.0048 4.21107L17.9627 4.16185C17.8641 4.04596 17.7327 3.96265 17.5859 3.9229C17.439 3.88315 17.2836 3.88884 17.14 3.9392L15.2345 4.61654C14.5314 4.03998 13.7463 3.58529 12.8978 3.26654L12.5298 1.27435C12.5021 1.12445 12.4294 0.986546 12.3214 0.878957C12.2134 0.771368 12.0752 0.699189 11.9252 0.672009L11.8619 0.660291C10.6408 0.439978 9.35641 0.439978 8.13531 0.660291L8.07203 0.672009C7.92202 0.699189 7.78384 0.771368 7.67583 0.878957C7.56782 0.986546 7.49511 1.12445 7.46734 1.27435L7.09703 3.27592C6.25536 3.59473 5.47156 4.04918 4.77672 4.62123L2.85719 3.9392C2.71367 3.88844 2.5581 3.88255 2.41116 3.92232C2.26422 3.96209 2.13286 4.04564 2.03453 4.16185L1.99234 4.21107C1.18081 5.16761 0.54807 6.26241 0.124376 7.4431L0.103282 7.50404C-0.00218645 7.79701 0.0845324 8.12513 0.321251 8.32904L1.87516 9.6556C1.8025 10.0962 1.76734 10.5462 1.76734 10.9939C1.76734 11.4439 1.8025 11.8939 1.87516 12.3322L0.321251 13.6587C0.20546 13.7579 0.122587 13.8899 0.083651 14.0372C0.0447156 14.1846 0.0515627 14.3403 0.103282 14.4837L0.124376 14.5447C0.548595 15.7259 1.17672 16.8158 1.99234 17.7767L2.03453 17.8259C2.1331 17.9418 2.26446 18.0251 2.41132 18.0649C2.55817 18.1046 2.71363 18.0989 2.85719 18.0486L4.77672 17.3665C5.47516 17.9408 6.25563 18.3954 7.09703 18.7119L7.46734 20.7134C7.49511 20.8633 7.56782 21.0012 7.67583 21.1088C7.78384 21.2164 7.92202 21.2886 8.07203 21.3158L8.13531 21.3275C9.36763 21.549 10.6296 21.549 11.8619 21.3275L11.9252 21.3158C12.0752 21.2886 12.2134 21.2164 12.3214 21.1088C12.4294 21.0012 12.5021 20.8633 12.5298 20.7134L12.8978 18.7212C13.7459 18.4033 14.5355 17.9471 15.2345 17.3712L17.14 18.0486C17.2835 18.0993 17.4391 18.1052 17.586 18.0654C17.733 18.0257 17.8643 17.9421 17.9627 17.8259L18.0048 17.7767C18.8205 16.8134 19.4486 15.7259 19.8728 14.5447L19.8939 14.4837C19.9994 14.1954 19.9127 13.8673 19.6759 13.6634ZM16.4767 9.9181C16.5353 10.272 16.5658 10.6353 16.5658 10.9986C16.5658 11.3619 16.5353 11.7251 16.4767 12.079L16.322 13.0189L18.0728 14.5165C17.8074 15.128 17.4724 15.7068 17.0744 16.2415L14.8994 15.4704L14.1634 16.0751C13.6033 16.5345 12.9798 16.8954 12.3048 17.1486L11.4119 17.4837L10.9923 19.7572C10.3304 19.8322 9.6621 19.8322 9.00016 19.7572L8.58063 17.479L7.69469 17.1392C7.02672 16.8861 6.40563 16.5251 5.85016 16.0681L5.11422 15.4611L2.92516 16.2392C2.52672 15.7025 2.19391 15.1236 1.92672 14.5142L3.69625 13.0025L3.54391 12.065C3.48766 11.7158 3.45719 11.3548 3.45719 10.9986C3.45719 10.64 3.48531 10.2814 3.54391 9.93217L3.69625 8.99467L1.92672 7.48295C2.19156 6.87123 2.52672 6.29467 2.92516 5.75795L5.11422 6.53607L5.85016 5.92904C6.40563 5.47201 7.02672 5.11107 7.69469 4.85795L8.58297 4.52279L9.0025 2.24467C9.66109 2.16967 10.3338 2.16967 10.9947 2.24467L11.4142 4.5181L12.3072 4.85326C12.9798 5.10638 13.6056 5.46732 14.1658 5.9267L14.9017 6.53138L17.0767 5.76029C17.4752 6.29701 17.808 6.87592 18.0752 7.48529L16.3244 8.98295L16.4767 9.9181ZM10.0009 6.6392C7.72281 6.6392 5.87594 8.48607 5.87594 10.7642C5.87594 13.0423 7.72281 14.8892 10.0009 14.8892C12.2791 14.8892 14.1259 13.0423 14.1259 10.7642C14.1259 8.48607 12.2791 6.6392 10.0009 6.6392ZM11.8572 12.6204C11.6137 12.8646 11.3244 13.0582 11.0058 13.1902C10.6873 13.3221 10.3457 13.3897 10.0009 13.3892C9.30016 13.3892 8.64156 13.115 8.14469 12.6204C7.90052 12.377 7.70691 12.0876 7.57498 11.7691C7.44304 11.4505 7.3754 11.109 7.37594 10.7642C7.37594 10.0634 7.65016 9.40482 8.14469 8.90795C8.64156 8.41107 9.30016 8.1392 10.0009 8.1392C10.7017 8.1392 11.3603 8.41107 11.8572 8.90795C12.1014 9.15141 12.295 9.44075 12.4269 9.75932C12.5588 10.0779 12.6265 10.4194 12.6259 10.7642C12.6259 11.465 12.3517 12.1236 11.8572 12.6204Z"
      fill="currentColor"
    />
  </svg>
`;

export const forwardIcon = html`
  <svg
    width="22"
    height="22"
    viewBox="0 0 22 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M14.6258 10.8489L8.86013 6.67701C8.83211 6.65666 8.799 6.64448 8.76448 6.6418C8.72996 6.63912 8.69537 6.64605 8.66454 6.66183C8.63372 6.6776 8.60787 6.7016 8.58985 6.73117C8.57183 6.76074 8.56236 6.79472 8.56247 6.82935V7.92856C8.56247 8.16763 8.67731 8.39497 8.87185 8.53559L12.2797 11.0012L8.87185 13.4668C8.67731 13.6075 8.56247 13.8324 8.56247 14.0739V15.1731C8.56247 15.3254 8.73591 15.4145 8.86013 15.3254L14.6258 11.1536C14.7289 11.0786 14.7289 10.9239 14.6258 10.8489Z"
      fill="#262626"
    />
    <path
      d="M11 0.500122C5.20156 0.500122 0.5 5.20168 0.5 11.0001C0.5 16.7986 5.20156 21.5001 11 21.5001C16.7984 21.5001 21.5 16.7986 21.5 11.0001C21.5 5.20168 16.7984 0.500122 11 0.500122ZM11 19.7189C6.18594 19.7189 2.28125 15.8142 2.28125 11.0001C2.28125 6.18606 6.18594 2.28137 11 2.28137C15.8141 2.28137 19.7188 6.18606 19.7188 11.0001C19.7188 15.8142 15.8141 19.7189 11 19.7189Z"
      fill="#262626"
    />
  </svg>
`;

export const infoIcon = html`
  <svg
    width="22"
    height="22"
    viewBox="0 0 22 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M11 0.500122C5.20156 0.500122 0.5 5.20168 0.5 11.0001C0.5 16.7986 5.20156 21.5001 11 21.5001C16.7984 21.5001 21.5 16.7986 21.5 11.0001C21.5 5.20168 16.7984 0.500122 11 0.500122ZM11 19.7189C6.18594 19.7189 2.28125 15.8142 2.28125 11.0001C2.28125 6.18606 6.18594 2.28137 11 2.28137C15.8141 2.28137 19.7188 6.18606 19.7188 11.0001C19.7188 15.8142 15.8141 19.7189 11 19.7189Z"
      fill="#262626"
    />
    <path
      d="M9.875 6.87512C9.875 7.17349 9.99353 7.45964 10.2045 7.67062C10.4155 7.8816 10.7016 8.00012 11 8.00012C11.2984 8.00012 11.5845 7.8816 11.7955 7.67062C12.0065 7.45964 12.125 7.17349 12.125 6.87512C12.125 6.57675 12.0065 6.29061 11.7955 6.07963C11.5845 5.86865 11.2984 5.75012 11 5.75012C10.7016 5.75012 10.4155 5.86865 10.2045 6.07963C9.99353 6.29061 9.875 6.57675 9.875 6.87512ZM11.5625 9.50012H10.4375C10.3344 9.50012 10.25 9.5845 10.25 9.68762V16.0626C10.25 16.1657 10.3344 16.2501 10.4375 16.2501H11.5625C11.6656 16.2501 11.75 16.1657 11.75 16.0626V9.68762C11.75 9.5845 11.6656 9.50012 11.5625 9.50012Z"
      fill="#262626"
    />
  </svg>
`;

export const attentionIcon = html`
  <svg
    width="22"
    height="22"
    viewBox="0 0 22 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M11 0.500122C5.20156 0.500122 0.5 5.20168 0.5 11.0001C0.5 16.7986 5.20156 21.5001 11 21.5001C16.7984 21.5001 21.5 16.7986 21.5 11.0001C21.5 5.20168 16.7984 0.500122 11 0.500122ZM11 19.7189C6.18594 19.7189 2.28125 15.8142 2.28125 11.0001C2.28125 6.18606 6.18594 2.28137 11 2.28137C15.8141 2.28137 19.7188 6.18606 19.7188 11.0001C19.7188 15.8142 15.8141 19.7189 11 19.7189Z"
      fill="#262626"
    />
    <path
      d="M9.875 15.1251C9.875 15.4235 9.99353 15.7096 10.2045 15.9206C10.4155 16.1316 10.7016 16.2501 11 16.2501C11.2984 16.2501 11.5845 16.1316 11.7955 15.9206C12.0065 15.7096 12.125 15.4235 12.125 15.1251C12.125 14.8268 12.0065 14.5406 11.7955 14.3296C11.5845 14.1186 11.2984 14.0001 11 14.0001C10.7016 14.0001 10.4155 14.1186 10.2045 14.3296C9.99353 14.5406 9.875 14.8268 9.875 15.1251ZM10.4375 12.5001H11.5625C11.6656 12.5001 11.75 12.4157 11.75 12.3126V5.93762C11.75 5.8345 11.6656 5.75012 11.5625 5.75012H10.4375C10.3344 5.75012 10.25 5.8345 10.25 5.93762V12.3126C10.25 12.4157 10.3344 12.5001 10.4375 12.5001Z"
      fill="#262626"
    />
  </svg>
`;

export const copyIcon = html`
  <svg
    width="18"
    height="22"
    viewBox="0 0 18 22"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M16.5 0.500122H3.9375C3.83437 0.500122 3.75 0.584497 3.75 0.687622V2.00012C3.75 2.10325 3.83437 2.18762 3.9375 2.18762H15.5625V18.3126C15.5625 18.4157 15.6469 18.5001 15.75 18.5001H17.0625C17.1656 18.5001 17.25 18.4157 17.25 18.3126V1.25012C17.25 0.835278 16.9148 0.500122 16.5 0.500122ZM13.5 3.50012H1.5C1.08516 3.50012 0.75 3.83528 0.75 4.25012V16.6884C0.75 16.8876 0.829687 17.0775 0.970312 17.2181L5.03203 21.2798C5.08359 21.3314 5.14219 21.3736 5.20547 21.4087V21.4532H5.30391C5.38594 21.4837 5.47266 21.5001 5.56172 21.5001H13.5C13.9148 21.5001 14.25 21.165 14.25 20.7501V4.25012C14.25 3.83528 13.9148 3.50012 13.5 3.50012ZM5.20312 19.0673L3.18516 17.047H5.20312V19.0673ZM12.5625 19.8126H6.70312V16.4845C6.70312 15.9665 6.28359 15.547 5.76562 15.547H2.4375V5.18762H12.5625V19.8126Z"
      fill="currentColor"
    />
  </svg>
`;

export const caretDownIcon = html`
  <svg
    width="20"
    height="20"
    viewBox="0 0 21 21"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <g fill="none" stroke="currentColor" stroke-width="1">
      <circle cx="10" cy="10" r="9" />
      <path d="M6 8.5 L10 12.5 L14 8.5" />
    </g>
  </svg>
`;

export const arrowRight = html`
  <svg
    viewBox="0 0 23 21"
    xmlns="http://www.w3.org/2000/svg"
    class="icon icon--arrow-right"
  >
    <style>
      .icon--arrow-right g {
        stroke-width: var(--stroke-width, 1);
      }
    </style>
    <g vector-effect="non-scaling-stroke" fill="none" stroke="currentColor">
      <path d="M 15.5 5 L 21 10 L 15.5 15" />
      <line x1="0" y1="10" x2="21" y2="10" />
    </g>
  </svg>
`;

export const spinner = html`
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
    <circle
      fill="none"
      stroke="currentColor"
      stroke-width="8"
      cx="50"
      cy="50"
      r="44"
      style="opacity:0.2;"
    ></circle>
    <circle
      fill="currentColor"
      stroke="#fff"
      stroke-width="1"
      cx="8"
      cy="54"
      r="10"
    ></circle>
  </svg>
`;

export const browserIcon = html` <svg
  xmlns="http://www.w3.org/2000/svg"
  fill="none"
  width="53"
  height="52"
  viewBox="0 0 100 82"
>
  <path
    fill="currentColor"
    d="M0 4a4 4 0 0 1 4-4h92a4 4 0 0 1 4 4v74a4 4 0 0 1-4 4H4a4 4 0 0 1-4-4V4Zm4 0v74h92V4H4Z"
  />
  <path fill="currentColor" d="M4 4h92v18H4V4Z" />
  <path
    fill="currentColor"
    fill-rule="evenodd"
    d="M31 55a7 7 0 1 0 0-14 7 7 0 0 0 0 14Zm0 4c6.075 0 11-4.925 11-11s-4.925-11-11-11-11 4.925-11 11 4.925 11 11 11Z"
  />
  <path fill="currentColor" d="M80 50H40v-4h40v4Z" />
  <path fill="currentColor" d="M76 61V46h4v15h-4Z" />
  <path fill="currentColor" d="M67 59V46h4v13h-4Z" />
</svg>`;

export const dropdownIcon = html`<i
  class="c-icon c-icon--more c-icon--outlined"
>
  <i class="c-icon--more__dot"></i>
  <i class="c-icon--more__dot"></i>
  <i class="c-icon--more__dot"></i>
</i>`;

export const lockIcon = html`<svg
  xmlns="http://www.w3.org/2000/svg"
  viewBox="0 0 21 21"
>
  <g fill="none" fill-rule="evenodd" transform="translate(4 1)">
    <path
      stroke="currentColor"
      stroke-linecap="round"
      stroke-linejoin="round"
      d="m2.5 8.5-.006-1.995C2.487 2.502 3.822.5 6.5.5s4.011 2.002 4 6.005V8.5m-8 0h8.023a2 2 0 0 1 1.994 1.85l.006.156-.017 6a2 2 0 0 1-2 1.994H2.5a2 2 0 0 1-2-2v-6a2 2 0 0 1 2-2z"
    />
  </g>
</svg>`;
