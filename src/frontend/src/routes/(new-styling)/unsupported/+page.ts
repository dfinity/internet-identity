import { isNullish } from "@dfinity/utils";
import { PageLoad } from "../$types";

export const load: PageLoad = ({
  url,
}): { redirectUrl: string | null; noRedirect: boolean } => {
  const noRedirect = url.searchParams.get("noRedirect") === "true";
  const encodedRedirect = url.searchParams.get("redirect");

  // If noRedirect is set, skip redirect processing
  if (noRedirect) {
    return {
      redirectUrl: null,
      noRedirect: true,
    };
  }

  if (isNullish(encodedRedirect) || encodedRedirect === "") {
    return {
      redirectUrl: null,
      noRedirect: false,
    };
  }

  try {
    const decodedUrl = decodeURIComponent(encodedRedirect);
    return {
      redirectUrl: decodedUrl,
      noRedirect: false,
    };
  } catch (error) {
    // If decoding fails, treat it as if no redirect was provided
    console.error("Failed to decode redirect URL:", error);
    return {
      redirectUrl: null,
      noRedirect: false,
    };
  }
};
