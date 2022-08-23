export const validateAlias = (
  {
    valueMissing,
    patternMismatch,
  }: { valueMissing: boolean; patternMismatch: boolean },
  value: string
): string => {
  let message = "";
  if (valueMissing) {
    message = "Please fill out this field.";
  } else if (patternMismatch) {
    if (value.startsWith(" ")) {
      message = "Name can't start with a space.";
    } else if (
      value.endsWith(" ") ||
      value.endsWith("-") ||
      value.endsWith("_")
    ) {
      message = "Name can't end with a space, hyphen or underscore.";
    } else {
      message = "Name can't contain special characters.";
    }
  }
  return message;
};
