// TODO ;explain why we split

export const questions = {
  windowsHello: {
    question: "Does Internet Identity support Windows Hello?",
    anchor: "windows-hello",
    answer: "Yes! Internet Identity supports authenticating via Windows Hello. If Windows Hello is set up on your PR then Internet Identity will offer you to authenticate through Windows Hello.",
  },
  lostDevice: {
    question: "If I lose my device, can I still use Internet Identity?",
    anchor: "lost-device",
    answer:
      "If you have an Identity Anchor tied to only one device and you lose that one device, you will be locked out. As a best practice, we recommend adding multiple devices multiple devices and recovery mechanisms to every Identity Anchor.", // TODO links
  },
  moreDevices: {
    question: "How do I add more devices to my Identity Anchor?",
    anchor: "more-devices",
    answer:
      "To add more devices to an existing Identity Anchor, please see the guide here.",
  },
  canIBeTracked: {
    question: "Can I be tracked across DApps with Internet Identity?",
    anchor: "can-i-be-tracked",
    answer: "No. Internet Identity issues a different Principal ID (a \"pseudonym\") for each DApp that you authenticate for using Internet Identity. Since the pseudonyms Internet Identity generates for you are different for each DApp, the DApps cannot use pseudonyms to track you.",
  },
};
