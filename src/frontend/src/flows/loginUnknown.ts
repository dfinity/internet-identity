import { blobFromUint8Array, derBlobFromBlob } from "@dfinity/agent";
import { render, html } from "lit-html";
import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";
import { IDPActor } from "../utils/idp_actor";
import { setUserId } from "../utils/userId";
import { withLoader } from "../components/loader";
import { register } from "./register";
import { displayAddDeviceLink } from "./displayAddDeviceLink";

const pageContent = () => html`
  <style>
#registerUserNumber:focus {
  box-sizing: border-box;
  border: double 2px transparent;
  border-radius: 4px;
  border-image-slice: 1;
  outline: none;
  border-image-source: linear-gradient(
    270.05deg,
    #29abe2 10.78%,
    #522785 22.2%,
    #ed1e79 42.46%,
    #f15a24 59.41%,
    #fbb03b 77.09%
  );
}

#newUser {
  font-size: 0.875rem;
  margin-top: 4rem;
}
#newUser a {
  font-weight: 600;
  text-decoration: none;
  color: black;
}
  </style>
  <div class="container">
    <h2 id="loginWelcome">Welcome to<br>Internet Identity!</h2>
    <p>Using your saved User ID you can login or link a new device</p>
    <input type="text" id="registerUserNumber" placeholder="Enter User ID">
    <button type="button" id="loginButton" class="primary">Login</button>
    <p>First time using this device?</p>
    <button type="button" id="dialogTrigger">Link this device</button>
    <div id="newUser">
      New user? <button id="registerButton" class="linkStyle">Get Identity</button>
    </div>
  </div>`;

export type LoginResult =
  | {
    tag: "ok";
    userId: bigint;
    connection: IDPActor;
  }
  | {
    tag: "err";
    message: string;
    detail: string;
  };

export const loginUnknown = async (): Promise<LoginResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return new Promise((resolve) => {
    initLogin(resolve);
    initLinkDevice();
    initRegister(resolve);
  });
};

const initRegister = (resolve) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = async () => {
    const res = await register();
    if (res === null) {
      window.location.reload()
    } else {
      resolve(res)
    }
  }
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
      withLoader(() =>
        IDPActor.login(userId).then((connection) => {
          setUserId(userId);
          resolve({ tag: "ok", userId, connection });
        }));
    } else {
      resolve({
        tag: "err",
        message: "Please enter a valid userId",
        detail: `${userId} doesn't parse as a number`,
      });
    }
  };
};

const initLinkDevice = () => {
  const dialogTrigger = document.getElementById(
    "dialogTrigger"
  ) as HTMLButtonElement;

  dialogTrigger.onclick = async () => {
    const userIdInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;
    let loginInterval: number;

    const userId = BigInt(userIdInput.value);
    if (userId) {
      const { link, publicKey } = await generateAddDeviceLink(userId);
      displayAddDeviceLink(link);
      loginInterval = window.setInterval(async () => {
        console.log("checking if authenticated");
        try {
          let devices = await IDPActor.lookup(userId);
          let matchedDevice = devices.find((deviceData) =>
            derBlobFromBlob(
              blobFromUint8Array(Buffer.from(deviceData.pubkey))
            ).equals(publicKey)
          );
          if (matchedDevice !== undefined) {
            window.clearInterval(loginInterval);
            setUserId(userId);
            window.location.reload();
          }
        } catch (error) {
          console.error(error);
        }
      }, 2500);
    } else {
      // TODO: Display an error next to the UserID input?
      console.error("Not a valid user id");
    }
  };
};
