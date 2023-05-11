export const handleKeyPress = ({
  $event: { code },
  callback,
}: {
  $event: KeyboardEvent;
  callback: () => void;
}) => {
  if (!["Enter", "Space"].includes(code)) {
    return;
  }

  callback();
};
