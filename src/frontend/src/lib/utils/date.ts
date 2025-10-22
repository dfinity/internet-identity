/**
 * Converts a timestamp in milliseconds to a date string in the format dd.mm.yyyy.
 * @param millis
 * @returns dd.mm.yyyy
 */
export const toDate = (millis: number): string => {
  const date = new Date(millis);
  const day = String(date.getDate()).padStart(2, "0");
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const year = date.getFullYear();
  return `${day}.${month}.${year}`;
};
