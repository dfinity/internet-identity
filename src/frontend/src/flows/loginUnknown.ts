import { blobFromUint8Array, derBlobFromBlob } from "@dfinity/agent";
import { render, html } from "lit-html";
import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";
import { IDPActor } from "../utils/idp_actor";
import { setUserId } from "../utils/userId";

const pageContent = () => html`<h1>Internet Identity</h1>
        <div class="spacer"></div>
        <h3>Are you an existing user?</h3>
        <section id="userIdSection">
        <p>Enter your saved User #</p>
        <fieldset>
          <label for="registerUserNumber">User #</label>
          <input type="text" id="registerUserNumber" name="registerUserNumber" />
        </fieldset>
        </section>
        <button type="button" id="dialogTrigger" aria-controls="loginDialog">
          Link new Device
        </button>
        <button type="button" id="loginButton" aria-controls="loginDialog">
          Login with existing Device
        </button>
        <div class="spacer"></div>
        <h3>If this is your first time using the Internet Identity</h3>
        <form id="registerForm">
          <label for="registerAlias">What should we call this device? </label>
          <input
            type="text"
            name="registerAlias"
            id="registerAlias"
            required
            placeholder="device name"
          />
          <button type="submit" id="register-identity">Click here to register</button>
        </form>
        <web-dialog id="linkDeviceDialog">
          <button type="button" aria-label="close dialog" id="closeDialog">
            <svg
              id="icon_close"
              data-name="icon/close"
              xmlns="http://www.w3.org/2000/svg"
              width="14"
              height="14"
              viewBox="0 0 14 14"
            >
              <rect
                id="Rectangle_91"
                data-name="Rectangle 91"
                width="14"
                height="14"
                fill="none"
              />
              <path
                id="Line"
                d="M0,0,9,9"
                transform="translate(2.5 2.5)"
                fill="none"
                stroke="gray"
                stroke-miterlimit="10"
                stroke-width="3"
              />
              <path
                id="Line-2"
                data-name="Line"
                d="M9,0,0,9"
                transform="translate(2.5 2.5)"
                fill="none"
                stroke="gray"
                stroke-miterlimit="10"
                stroke-width="3"
              />
            </svg>
          </button>
          <p>
            Copy the URL below and open it in your already authenticated browser.
            This page will automatically update when you have succeeded.
          </p>
          <input type="text" id="addDeviceLink" readonly />
        </web-dialog>
        <div id="notification"></div>`;


export type LoginResult = {
    tag: "ok",
    userId: bigint,
    connection: IDPActor
} | {
    tag: "err",
    message: string,
    detail: string,
}

export const loginUnknown = async (): Promise<LoginResult> => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(), container);
    return new Promise(resolve => {
        initRegisterForm(resolve);
        initLogin(resolve);
        initLinkDevice();
    })
};

const initRegisterForm = (resolve: (loginResult: LoginResult) => void) => {
    const form = document.getElementById("registerForm") as HTMLFormElement;
    const submitButton = form.querySelector(
        'button[type="submit"]'
    ) as HTMLButtonElement;

    const handleSubmit = (e) => {
        // Enter pending state
        e.preventDefault();
        submitButton.setAttribute("disabled", "true");

        // Read values from inputs
        const registerAlias = form.querySelector(
            "#registerAlias"
        ) as HTMLInputElement;

        // Send values through actor
        IDPActor.register(registerAlias.value).then(({ connection, userId }) => {
            setUserId(userId);
            resolve({ tag: "ok", userId, connection })
        }).catch((err) => {
            console.error(err);
            resolve({ tag: "err", message: "Failed to register a new Internet Identity.", detail: err.toString() })
        });
        return false;
    };

    form.onsubmit = handleSubmit;
}

const initLogin = (resolve) => {
    const userIdInput = document.getElementById(
        "registerUserNumber"
    ) as HTMLInputElement;
    const loginButton = document.getElementById(
        "loginButton"
    ) as HTMLButtonElement;

    loginButton.onclick = () => {
        const userId = BigInt(userIdInput.value);
        if (userId) {
            IDPActor.reconnect(userId).then(connection => {
                setUserId(userId);
                resolve({ tag: "ok", userId, connection });
            });
        } else {
            resolve({ tag: "err", message: "Please enter a valid userId", detail: `${userId} doesn't parse as a number` })
        }
    }
};

const initLinkDevice = () => {
    const dialog = document.getElementById("linkDeviceDialog") as HTMLDialogElement;
    const dialogTrigger = document.getElementById(
        "dialogTrigger"
    ) as HTMLButtonElement;

    dialogTrigger.onclick = () => {
        const userIdInput = document.getElementById(
            "registerUserNumber"
        ) as HTMLInputElement;
        let loginInterval: number;

        const userId = BigInt(userIdInput.value);
        if (userId) {
            generateAddDeviceLink(userId).then(({ link, publicKey }) => {
                dialog.open = true;
                setUserId(userId);
                const addDeviceLink = document.getElementById(
                    "addDeviceLink"
                ) as HTMLInputElement;
                addDeviceLink.value = link;

                loginInterval = window.setInterval(async () => {
                    console.log("checking if authenticated");
                    try {
                        let devices = await IDPActor.lookup(userId);
                        let matchedDevice = devices.find(deviceData =>
                            derBlobFromBlob(blobFromUint8Array(Buffer.from(deviceData.pubkey))).equals(publicKey)
                        );
                        if (matchedDevice !== undefined) {
                            window.clearInterval(loginInterval)
                            window.location.reload()
                        }
                    } catch (error) {
                        console.error(error);
                    }
                }, 2500);
            });
        } else {
            console.error("Not a valid user id");
        }
    }
};
