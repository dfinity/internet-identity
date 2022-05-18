export const formatSessionValidity = (
  maxTimeToLive: bigint | undefined,
  defaultValue: number
): string => {
  const seconds =
    maxTimeToLive !== undefined
      ? Math.floor(Number(maxTimeToLive / BigInt(1_000_000_000)))
      : defaultValue;
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);
  if (days > 0) {
    return formatWithSecondaryTimeUnit(days, "day", hours % 24, "hour");
  }
  if (hours > 0) {
    return formatWithSecondaryTimeUnit(hours, "hour", minutes % 60, "minute");
  }
  if (minutes > 0) {
    return formatWithSecondaryTimeUnit(
      minutes,
      "minute",
      seconds % 60,
      "second"
    );
  }
  return formatTimeUnit(seconds, "second");
};

const formatWithSecondaryTimeUnit = (
  primaryAmount: number,
  primaryUnit: string,
  secondaryAmount: number,
  secondaryUnit: string
) => {
  let result = formatTimeUnit(primaryAmount, primaryUnit);
  if (secondaryAmount > 0) {
    result = result + `, ${formatTimeUnit(secondaryAmount, secondaryUnit)}`;
  }
  return result;
};

const formatTimeUnit = (amount: number, unit: string) => {
  const roundedAmount = Math.floor(amount);
  return `${roundedAmount} ${unit}${roundedAmount > 1 ? "s" : ""}`;
};
