import {
  BinaryBlob,
  blobFromHex,
  derBlobFromBlob,
  DerEncodedBlob,
} from "@dfinity/agent";
import { identityListItem } from "../../templates/identityListItem";
import idp_actor from "../utils/idp_actor";

export const initManageIdentities = () => {
  // TODO - Check alias for current identity, and populate #nameSpan
  // TODO - Lookup identities then render them based on the result.

  if (!idp_actor.userId) {
    // If we haven't established a userId, we need to authenticate.
    location.assign(location.href.replace("manage", "index"));
  }
  // Check URL if user has pasted in an Add Identity link

  const url = new URL(document.URL);
  const newDevice = url.searchParams.get("device");
  if (newDevice !== null) {
    const parsedParams = parseNewDeviceParam(newDevice);
    if (parsedParams !== null) {
      const { userId, publicKey, rawId } = parsedParams;
      console.log("Adding new device with:", parsedParams);
      // TODO: Prompt the user for an alias and let them add the new device to their
      // existing identity
    }
  }
  renderIdentities();
};

const parseNewDeviceParam = (
  param: string
): { userId: BigInt; publicKey: DerEncodedBlob; rawId: BinaryBlob } | null => {
  const segments = param.split(";");
  if (segments.length !== 3) {
    // TODO: Decent error handling
    console.error("This is not a valid pasted link");
    return null;
  }
  const userId = BigInt(segments[0]);
  const publicKey = derBlobFromBlob(blobFromHex(segments[1]));
  const rawId = blobFromHex(segments[2]);
  return { userId, publicKey, rawId };
};

const renderIdentities = async () => {
  const identityList = document.getElementById("identityList") as HTMLElement;
  identityList.innerHTML = ``;

  const identities = await idp_actor.lookup();

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const [alias, publicKey] = identity;
    const identityElement = document.createElement("li");
    identityElement.innerHTML = identityListItem(alias);
    bindRemoveListener(identityElement, publicKey);
    list.appendChild(identityElement);
  });

  identityList.appendChild(list);
};

const bindRemoveListener = (listItem, publicKey) => {
  listItem.querySelector("button").onclick = () => alert(publicKey);

  // idp_actor.remove(publicKey);
};
