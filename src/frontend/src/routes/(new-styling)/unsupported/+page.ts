import { PageLoad } from "../$types";

export const load: PageLoad = ({ url }): { redirectUrl: string | null } => {
  const encodedRedirect = url.searchParams.get("redirect");

  if (!encodedRedirect) {
    return {
      redirectUrl: null,
    };
  }

  try {
    const decodedUrl = decodeURIComponent(encodedRedirect);
    return {
      redirectUrl: decodedUrl,
    };
  } catch (error) {
    // If decoding fails, treat it as if no redirect was provided
    console.error("Failed to decode redirect URL:", error);
    return {
      redirectUrl: null,
    };
  }
};
