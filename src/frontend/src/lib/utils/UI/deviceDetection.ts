// utils/deviceDetection.ts
export function isMobileDevice(): boolean {
  // Check if window is defined (for SSR compatibility)
  if (typeof window === "undefined") return false;

  // Check for mobile user agent patterns
  const mobileRegex =
    /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i;
  const userAgent = navigator.userAgent;

  // Additional check for screen width (optional)
  const isMobileWidth = window.innerWidth <= 768; // You can adjust this breakpoint

  return mobileRegex.test(userAgent) || isMobileWidth;
}

// Alternative if you only want to check screen size
export function isDesktopViewport(): boolean {
  if (typeof window === "undefined") return true;
  return window.innerWidth > 768; // You can adjust this breakpoint
}
