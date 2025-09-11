import type { PageLoad } from "./$types";
import { isNullish } from "@dfinity/utils";
import { redirect } from "@sveltejs/kit";

export const load: PageLoad = ({ url }) => {
  console.log("in da load authorize selected");
  const authMethod = url.searchParams.get("authMethod");
  console.log("authMethod", authMethod);

  // If authMethod is missing, redirect back to the main authorize route
  if (isNullish(authMethod)) {
    throw redirect(307, "/authorize");
  }

  return {
    authMethod,
  };
};
