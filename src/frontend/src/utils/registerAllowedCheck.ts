import { LoginFlowResult } from "./flowResult";
import { register } from "../flows/register";
import { registerDisabled } from "../flows/registerDisabled";
import { Connection } from "../utils/iiConnection";

/** Check that the current origin is not the explicit canister id or a raw url.
 *  Explanation why we need to do this:
 *  https://forum.dfinity.org/t/internet-identity-deprecation-of-account-creation-on-all-origins-other-than-https-identity-ic0-app/9694
 **/
function isRegistrationAllowed() {
  return !/(^https:\/\/rdmx6-jaaaa-aaaaa-aaadq-cai\.ic0\.app$)|(.+\.raw\..+)/.test(
    window.origin
  );
}

export const registerIfAllowed = async (
  connection: Connection
): Promise<LoginFlowResult> => {
  return isRegistrationAllowed()
    ? register({ connection })
    : registerDisabled();
};
