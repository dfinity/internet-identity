import {
  BinaryBlob,
  blobFromHex,
  derBlobFromBlob,
  DerEncodedBlob,
} from "@dfinity/agent";
import { identityListItem } from "../../templates/identityListItem";
import idp_actor from "../utils/idp_actor";
import { prompt } from "../utils/prompt";

export const initManageIdentities = () => {
  // TODO - Check alias for current identity, and populate #nameSpan

  if (idp_actor.userId === undefined) {
    // If we haven't established a userId, we need to authenticate.
    location.assign(location.href.replace("manage", "index"));
    return;
  }

  checkForAddUserHash();
  displayUserId(idp_actor.userId);
  renderIdentities();
};

const checkForAddUserHash = async () => {
  // Check URL if user has pasted in an Add Identity link
  const url = new URL(document.URL);

  const newDevice = url.hash?.split("device=")[1];
  if (!!newDevice) {
    const parsedParams = parseNewDeviceParam(newDevice);
    if (parsedParams !== null) {
      const { userId, publicKey, rawId } = parsedParams;
      console.log("Adding new device with:", parsedParams);
      let deviceName: string;
      try {
        deviceName = await prompt("What should we call this device?");
      } catch (error) {
        deviceName = "anonymous device";
      }
      await idp_actor.add(BigInt(userId), deviceName, publicKey, rawId);
      renderIdentities();
    }
  }
};

const displayUserId = (userId: BigInt) => {
  const userIdElem = document.getElementById("userIdSpan") as HTMLElement;
  userIdElem.innerHTML = userId.toString();
  const userIdSection = document.getElementById("userIdSection") as HTMLElement;
  userIdSection.classList.remove("hidden");
};

const parseNewDeviceParam = (
  param: string
): { userId: bigint; publicKey: DerEncodedBlob; rawId?: BinaryBlob } | null => {
  const segments = param.split(";");
  if (!(segments.length === 2 || segments.length === 3)) {
    // TODO: Decent error handling
    console.error("This is not a valid pasted link");
    return null;
  }
  const userId = BigInt(segments[0]);
  const publicKey = derBlobFromBlob(blobFromHex(segments[1]));
  const rawId = segments[2] ? blobFromHex(segments[2]) : undefined;
  return { userId, publicKey, rawId };
};
const renderIdentities = async () => {
  const identityList = document.getElementById("identityList") as HTMLElement;
  identityList.innerHTML = ``;

  const identities = await idp_actor.lookup();

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const identityElement = document.createElement("li");
    identityElement.className = "flex row justify-between";
    identityElement.innerHTML = identityListItem(identity.alias);
    bindRemoveListener(identityElement, identity.pubkey);
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
