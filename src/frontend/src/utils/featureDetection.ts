export const checkRequiredFeatures = async (
  url: URL
): Promise<true | string> => {
  if (window.PublicKeyCredential === undefined)
    return "window.PublicKeyCredential is not defined";
  if (url.hash === "#compatibilityNotice")
    return "Remove #compatibilityNotice from the URL and try again.";
  // For mobile devices we want to make sure we can use platform authenticators
  if (!navigator.userAgent.match(/(iPhone|iPod|iPad|Android)/)) return true;
  try {
    return (await PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable())
      ? true
      : "UserVerifyingPlatformAuthenticator is not available";
  } catch (error) {
    return `An error occured when checking for compatibility: ${error.message}`;
  }
};
