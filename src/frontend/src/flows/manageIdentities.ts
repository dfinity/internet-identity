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

  if (idp_actor.userId === undefined) {
    // If we haven't established a userId, we need to authenticate.
    location.assign(location.href.replace("manage", "index"));
  }

  checkForAddUserHash();
  renderIdentities();
};

const checkForAddUserHash = async () => {
  // Check URL if user has pasted in an Add Identity link
  const url = new URL(document.URL);

  const newDevice = url.hash?.split("device=")[1];
  if (!!newDevice) {
    const parsedParams = parseNewDeviceParam(newDevice);
    if (parsedParams !== null) {
      const { userId, rawId } = parsedParams;
      console.log("Adding new device with:", parsedParams);
      await idp_actor.add(
        BigInt(userId),
        prompt("What should we call this device?") ?? "anonymous device",
        rawId.toString()
      );
      renderIdentities();
    }
  }
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
    identityElement.className = "flex row justify-between";
    identityElement.innerHTML = identityListItem(alias);
    bindRemoveListener(identityElement, publicKey);
    list.appendChild(identityElement);
  });

  identityList.appendChild(list);
};

const bindRemoveListener = (listItem: HTMLElement, publicKey) => {
  const button = listItem.querySelector("button") as HTMLButtonElement;
  button.onclick = () => {
    // Make sure we're not removing our last identity
    const identities = document.querySelectorAll("#identityList li");

    if (identities.length <= 1) {
      const shouldProceed = confirm(
        "This will remove your only remaining identity and may impact your ability to log in to accounts you have linked"
      );
      if (!shouldProceed) {
        return;
      }
    }
    // Otherwise, remove identity
    idp_actor.remove(publicKey).then(() => {
      listItem.parentElement?.removeChild(listItem);
    });
  };
};
