export const handleKeyPress = ({
  e: { code },
  callback,
}: {
  e: KeyboardEvent;
  callback: () => void;
}) => {
  if (!["Enter", "Space"].includes(code)) {
    return;
  }

  callback();
};
