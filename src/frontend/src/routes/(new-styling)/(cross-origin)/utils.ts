interface ForwardedMessage {
  __ii_forwarded: {
    data: unknown;
    origin: string;
  };
}

export const isForwardedMessage = (
  event: MessageEvent,
): event is MessageEvent<ForwardedMessage> => {
  return (
    typeof event.data === "object" &&
    event.data !== null &&
    "__ii_forwarded" in event.data &&
    "data" in event.data.__ii_forwarded &&
    "origin" in event.data.__ii_forwarded &&
    typeof event.data.__ii_forwarded.origin === "string"
  );
};

export const forwardMessage = (
  data: unknown,
  origin: string,
): ForwardedMessage => ({
  __ii_forwarded: {
    data,
    origin,
  },
});
