import { redirect, type Load, type LoadEvent } from "@sveltejs/kit";

export const load: Load = (event: LoadEvent) => {
  // TODO: Remove when we launch lanidng page in id.ai
  const hostname = event.url.hostname;
  if (hostname === "id.ai") {
    throw redirect(307, "https://identity.internetcomputer.org");
  } else if (hostname === "beta.id.ai") {
    throw redirect(307, "https://beta.identity.internetcomputer.org");
  }
  return {};
};
