import {
  BinaryBlob,
  blobFromHex,
  derBlobFromBlob,
  DerEncodedBlob,
} from "@dfinity/agent";
import { identityListItem } from "../../templates/identityListItem";
import idp_actor, { IDPActor } from "../utils/idp_actor";
import { prompt } from "../components/prompt";
import { navigateTo } from "../utils/router";
import { getUserId } from "../utils/userId";

export const initManageIdentities = async () => {
  // TODO - Check alias for current identity, and populate #nameSpan
  const userId = getUserId();
  if (userId === undefined) {
    // If we haven't established a userId, we need to authenticate.
    navigateTo(location.href.replace("manage", "index"));
  } else {
    displayUserId(userId);

    // TODO: If this fails display an error message and suggest 
    // the user try again with a different user id?
    let connection: IDPActor;
    if (idp_actor.connection === undefined) {
      connection = await IDPActor.reconnect(userId);
    } else {
      connection = idp_actor.connection
    };
    renderIdentities(connection, userId);
    checkForAddUserHash(connection);
  }

};

const checkForAddUserHash = async (connection: IDPActor) => {
  // Check URL if user has pasted in an Add Identity link
  const url = new URL(document.URL);

  const newDevice = url.hash?.split("device=")[1];
  if (!!newDevice) {
    const parsedParams = parseNewDeviceParam(newDevice);
    if (parsedParams !== null) {
      const { userId, publicKey, rawId } = parsedParams;
      if (getUserId() !== userId) {
        // TODO: Here we're adding a device to our userId that 
        // was supposed to be added to a different one.
        // Display a proper error here
        throw Error(`Tried adding a device for user ${userId}, but current user is ${getUserId()}. Aborting`)
      }
      console.log("Adding new device with:", parsedParams);
      try {
        const deviceName = await prompt("What should we call this device?");
        await connection.add(userId, deviceName, publicKey, rawId);
      } catch (error) {
        // If anything goes wrong, or the user cancels we do _not_ want to add the device.
        console.log(`Canceled adding the device with ${error}`);
        // TODO: Clear the hash & Error page? Or redirect to manage?
        return
      }
      // TODO: Clear the hash
      renderIdentities(connection, userId);
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
const renderIdentities = async (connection, userId) => {
  const identityList = document.getElementById("identityList") as HTMLElement;
  identityList.innerHTML = ``;

  const identities = await IDPActor.lookup(userId);

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const identityElement = document.createElement("li");
    identityElement.className = "flex row justify-between";
    identityElement.innerHTML = identityListItem(identity.alias);
    bindRemoveListener(userId, connection, identityElement, identity.pubkey);
    list.appendChild(identityElement);
  });

  identityList.appendChild(list);
};

const bindRemoveListener = (userId: bigint, connection: IDPActor, listItem: HTMLElement, publicKey) => {
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
    connection.remove(userId, publicKey).then(() => {
      listItem.parentElement?.removeChild(listItem);
    });
  };
};
