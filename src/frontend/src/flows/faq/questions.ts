// The questions (and answers) displayed on the FAQ page. We split this into
// its own file because we test this with jest (to make sure anchors are
// unique) and jest is very unhappy when it encouters a file importing
// `lit-html`.
//
// Each question has a `question` (the actual question), an anchor (use as e.g.
// `/faq#anchor`, should be unique), answer and links (see types below). Due to
// the jest limitation above we can't really inline the links, because that
// would mean importing `lit-html` here...

interface Link {
  name: string;
  link: string;
}

export interface Question {
  question: string;
  anchor: string;
  answer: string;
  links: Array<Link>;
}

export const questions = {
  windowsHello: {
    question: "Does Internet Identity support Windows Hello?",
    anchor: "windows-hello",
    answer:
      "Yes! Internet Identity supports authenticating via Windows Hello. If Windows Hello is set up on your PC then Internet Identity will offer you to authenticate through Windows Hello.",
    links: [
      {
        name: "Windows Hello Guide for Internet Identity",
        link: "https://sdk.dfinity.org/docs/ic-identity-guide/hello-guide.html",
      },
    ],
  },
  lostDevice: {
    question: "If I lose my device, can I still use Internet Identity?",
    anchor: "lost-device",
    answer:
      "If you have an Identity Anchor tied to only one device and you lose that one device, you will be locked out. As a best practice, we recommend adding multiple devices multiple devices and recovery mechanisms to every Identity Anchor.",
    links: [
      {
        name: "Device Management How-To",
        link: "https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html",
      },
    ],
  },
  moreDevices: {
    question: "How do I add more devices to my Identity Anchor?",
    anchor: "more-devices",
    answer:
      "To add more devices to an existing Identity Anchor, please see the guide here:",
    links: [
      {
        name: "How to add a device",
        link: "https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html#_add_a_device",
      },
    ],
  },
  shareIIAnchor: {
    question:
      "Does Internet Identity share my anchor or personal information with DApps?",
    anchor: "share-ii-anchor",
    answer:
      'No. Internet Identity uses a different Principal (a "pseudonym") for each DApp that you authenticate to using Internet Identity. Since the pseudonyms Internet Identity generates for you are different for each DApp, DApps cannot use them to track you outside of their realm.',
    links: [],
  },
};
