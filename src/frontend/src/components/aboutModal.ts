import { html } from "lit-html";

export const aboutButtonContent = html`
  <button id="info-button" class="c-button info-button">ℹ</button>
`;

export const aboutModalContent = html`
  <div id="info-page">
    <div id="info-page-content" class="_c-card">
      <div id="info-page-1" class="info-page-shown">
        <p class="info-strap-paragraph">
          <b
            >Internet Identity is a new way of securely authenticating yourself
            to online services, including web3 services that run from
            blockchains such as the Internet Computer.</b
          >
        </p>
        <p>
          To authenticate you must first create an identity "anchor," which is
          just a simple number, and then associate devices you own with the
          anchor. For example, you can associate phones and laptops with an
          anchor.
        </p>
        <p>
          An identity anchor is roughly equivalent to a username, and its
          associated devices are roughly equivalent to different passwords that
          work with that username.
        </p>
        <p>
          For example, after entering an anchor, you might authenticate using
          your phone's Face ID or PIN keypad features, or using the fingerprint
          sensor on your laptop...
        </p>
      </div>
      <div id="info-page-2" class="info-page-hidden">
        <p>
          Remember, to use an identity anchor, you must have one of the
          associated devices in your possession, and you must still have control
          of that device, as these act as its valid passwords.
        </p>
        <p>
          When you use an anchor to authenticate (i.e. "sign-on") to a service,
          that service does not actually see your anchor number. Instead, the
          Internet Identity system presents the service with a unique pseudonym,
          which prevents you being tracked across the different services you
          use.
        </p>
        <p>
          Your devices authenticate to online services using "private keys" that
          they keep inside special secure hardware, which is built-in to modern
          devices such as phones and laptops, and "hardware wallets." Unlike
          traditional passwords, and private key "seed phrases," these cannot be
          stolen, because they cannot be extracted from the secure hardware...
        </p>
      </div>
      <div id="info-page-3" class="info-page-hidden">
        <p>
          You can create as many individual anchors as you need. For example,
          you might create one anchor for use with social media, and another for
          DeFi. You might associate your phones and laptops with the anchor you
          use for social media, and only a special hardware wallet (such as
          <a href="https://www.ledger.com/" target="_blank">Ledger</a>, or
          <a href="https://www.yubico.com/" target="_blank">YubiKey</a>) with
          the anchor you use for DeFi, which you might keep hidden in a
          fireproof safe, say.
        </p>
        <p>
          If you already have an identity anchor that you wish to use, but this
          device (or a hardware wallet) has not yet been associated with that
          anchor, you must first associate your device with that anchor.
          <a href=""
            >Click here to associate this device with the anchor you wish to
            use</a
          >.
        </p>
        <p>
          If you do not have an identity anchor, you must first create a new one
          that you can use to authenticate (i.e. "sign-on") to the service. This
          is a quick and easy process...
        </p>
      </div>
      <div id="info-page-4" class="info-page-hidden">
        <p>
          Remember that to use any anchor you will need at least one of its
          associated devices, which act as passwords. Therefore, consider adding
          two devices to your new anchor, such as a phone and a laptop, so that
          if you lose one device, you can still control it using the other (you
          can also associate new devices at any time later).
        </p>
        <p>
          If your new anchor will be very important, you can also create a
          "recovery seed phrase." This can be written down and hidden in a safe
          place, in case all your devices get lost.
        </p>
        <p>
          <a href=""
            >Click here to create a new identity anchor that you can use to
            authenticate to this service, and add device(s)</a
          >.
        </p>
        <p>
          <a href=""
            >Click here to recover access to an existing anchor when you lost
            its devices</a
          >.
        </p>
        <p>
          Visit
          <a href="https://identity.internetcomputer.org" target="_blank"
            >https://identity.internetcomputer.org</a
          >
          to manage your anchors.
        </p>
      </div>
    </div>
    <div class="info-paginate-buttons">
      <span id="info-page-prev-btn" class="pagination-btn-disabled"
        >&#9194;</span
      >&nbsp;&nbsp;<span id="info-page-next-btn" class="pagination-btn"
        >&#9193;</span
      >
    </div>
    <button id="info-close-button" class="info-close-button">&#10006;</button>
  </div>
`;

const infoPage = (): HTMLElement =>
  document?.getElementById("info-page") as HTMLElement;

let visible = false;
let help_page_curr = 1;
let help_page_count = 0; // init page count from html

export const initAboutModal = (): void => {
  visible = false;

  while (document.getElementById("info-page-" + (help_page_count + 1)))
    help_page_count++;

  document
    .getElementById("info-button")
    ?.addEventListener("click", toggleAboutModalVisibility);
  document
    .getElementById("info-close-button")
    ?.addEventListener("click", toggleAboutModalVisibility);

  document
    .getElementById("info-page-next-btn")
    ?.addEventListener("click", next_info_help_page);
  document
    .getElementById("info-page-prev-btn")
    ?.addEventListener("click", prev_info_help_page);
};

export const toggleAboutModalVisibility = (): void => {
  console.log("toggleAboutModalVisibility", visible);

  if (visible) {
    infoPage().style.display = "none";

    // rewind pages to 1, so ready to be opened again
    while (help_page_curr > 1) prev_info_help_page();
  } else {
    infoPage().style.display = "block";
  }

  visible = !visible;
};

// TODO: to refactor
// move to next page of info content
function next_info_help_page() {
  if (!document.getElementById("info-page-" + (help_page_curr + 1))) return;
  help_page_curr++;
  show_info_help_page(
    help_page_curr - 1,
    help_page_curr,
    help_page_curr == 2,
    help_page_curr == help_page_count
  );
}
// move to prev page of info content
function prev_info_help_page() {
  if (help_page_curr <= 1) return;
  help_page_curr--;
  show_info_help_page(
    help_page_curr + 1,
    help_page_curr,
    help_page_curr == 1,
    help_page_curr + 1 == help_page_count
  );
}
// render specified info content
function show_info_help_page(
  hide: number,
  show: number,
  togglePrev: boolean,
  toggleNext: boolean
) {
  // update page
  document
    .getElementById("info-page-" + hide)
    ?.classList.toggle("info-page-shown");
  document
    .getElementById("info-page-" + hide)
    ?.classList.toggle("info-page-hidden");

  document
    .getElementById("info-page-" + show)
    ?.classList.toggle("info-page-shown");
  document
    .getElementById("info-page-" + show)
    ?.classList.toggle("info-page-hidden");

  // update pagination buttons
  if (togglePrev) {
    document
      .getElementById("info-page-prev-btn")
      ?.classList.toggle("pagination-btn-disabled");
    document
      .getElementById("info-page-prev-btn")
      ?.classList.toggle("pagination-btn");
  }

  if (toggleNext) {
    document
      .getElementById("info-page-next-btn")
      ?.classList.toggle("pagination-btn-disabled");
    document
      .getElementById("info-page-next-btn")
      ?.classList.toggle("pagination-btn");
  }
}
