import { BinaryBlob, blobFromHex, derBlobFromBlob, DerEncodedBlob } from "@dfinity/agent";
import { render, html } from "lit-html";
import { confirm } from "../components/confirm";
import { prompt } from "../components/prompt";
import { IDPActor } from "../utils/idp_actor";

const pageContent = (userId: bigint) => html`
    <h1>Adding a new device to your Internet Identity: ${userId}</h1>
    <p>Warning about not clicking this button unless this link _really_ came from yourself</p>
    <button type="button" id="addDevice">Add new device</button>
    <div id="notification"></div>
`;

const afterAddPageContent = (alias: string) => html`
    <p>You've successfully added ${alias} as a new Device.</p>
    <p>You can now close this window and return to your other device.</p>
`;

export const addDevice = (userId: bigint, connection: IDPActor) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(userId), container);
    init(userId, connection);
};

const init = (userId: bigint, connection: IDPActor) => {
    const addDeviceButton = document.querySelector(
        "#addDevice"
    ) as HTMLButtonElement;
    addDeviceButton.onclick = async (ev) => {
        // Check URL if user has pasted in an Add Identity link
        const url = new URL(document.URL);
        const parsedParams = parseNewDeviceParam(url.hash?.split("device=")[1]);

        if (!!parsedParams) {
            const { userId: expectedUserId, publicKey, rawId } = parsedParams;
            if (expectedUserId !== userId) {
                // Here we're adding a device to our userId that was supposed to be added to a different one.
                await confirm(
                    "Tried adding a device for the wrong user id.",
                    `Current user is ${expectedUserId}, but current user is ${userId}. Please choose the correct user id when creating the add device link`
                )
                return
            }
            console.log("Adding new device with:", parsedParams);
            try {
                const deviceName = await prompt("What should we call this device?");
                await connection.add(userId, deviceName, publicKey, rawId);
                const container = document.getElementById("pageContent") as HTMLElement;
                // TODO: Clear the hash
                render(afterAddPageContent(deviceName), container);
            } catch (error) {
                // If anything goes wrong, or the user cancels we do _not_ want to add the device.
                await confirm("Failed to add the device.", `Canceled adding the device with ${error}`)
                // TODO: Clear the hash & Error page? Or redirect to manage?
                return
            }

        }
    }
}

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
