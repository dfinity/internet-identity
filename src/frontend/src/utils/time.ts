export const formatLastUsage = (timestamp: Date): string => {
  const now = new Date();
  const diffInMillis = timestamp.getTime() - now.getTime();
  const diffInDays = Math.round(diffInMillis / (1000 * 60 * 60 * 24));

  // If more than 25 days, use months
  if (Math.abs(diffInDays) >= 25) {
    const diffInMonths = Math.round(diffInDays / 30);
    return new Intl.RelativeTimeFormat("en", {
      numeric: "auto",
      style: "long",
    }).format(diffInMonths, "month");
  }

  const relativeTime = new Intl.RelativeTimeFormat("en", {
    numeric: "auto",
    style: "long",
  }).format(diffInDays, "day");

  // If within last 24 hours, append the time
  if (Math.abs(diffInDays) < 1) {
    const timeString = new Intl.DateTimeFormat("en", {
      hour: "numeric",
      minute: "numeric",
    }).format(timestamp);
    return `${relativeTime} at ${timeString}`;
  }

  return relativeTime;
};
