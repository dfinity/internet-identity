/**
 * Helper function to compare two URL origins to check if they're the same.
 * This normalizes the URLs and compares their protocol, hostname, and port.
 *
 * @param urlA - The first URL to compare
 * @param urlB - The second URL to compare
 * @returns True if both origins are the same, false otherwise
 */
export const isSameOrigin = (urlA: string, urlB: string): boolean => {
  try {
    // Parse URLs to get access to their components
    const a = new URL(urlA);
    const b = new URL(urlB);

    // Compare protocol, hostname and port
    return a.origin === b.origin;
  } catch (error) {
    // If URL parsing fails, do a direct string comparison
    console.warn(`Failed to parse URLs for comparison: ${error}`);
    return urlA === urlB;
  }
};
