import React from "react";
import { creationOptions } from "../utils/iiConnection";
import ReactDOM from "react-dom";
import { WebAuthnIdentity } from "@dfinity/identity";
import { hasOwnProperty } from "../utils/utils";
import { pickDeviceAlias } from "./addDevicePickAlias";
import {
  BinaryBlob,
  blobFromUint8Array,
  derBlobFromBlob,
  DerEncodedBlob,
} from "@dfinity/candid";
import {
  _SERVICE,
  PublicKey,
  SessionKey,
  CredentialId,
  UserNumber,
  FrontendHostname,
  Timestamp,
  DeviceData,
  ProofOfWork,
  RegisterResponse,
  GetDelegationResponse,
  Purpose,
  KeyType,
  DeviceKey,
} from "../../generated/internet_identity_types";

// The various error messages we may display

const displayFailedToAddNewDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message: (
      <span>
        We failed to add your new device.
        <br />
        If you're trying to add a device that is not attached to this machine
        try following the instructions at
        <br />
        <a
          target="_blank"
          href="https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html#_add_a_device"
        >
          https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html#_add_a_device
        </a>
      </span>
    ),
    detail: error.message,
    primaryButton: "Back to manage",
  });

const displayFailedToAddTheDevice = (error: Error) =>
  displayError({
    title: "Failed to add the new device",
    message:
      "We failed to add the new device to this Identity Anchor. Please try again",
    detail: error.message,
    primaryButton: "Back to manage",
  });

const displayFailedToListDevices = (error: Error) =>
  displayError({
    title: "Failed to list your devices",
    message:
      "An unexpected error occurred when displaying your devices. Please try again",
    detail: error.toString(),
    primaryButton: "Try again",
  });

// The styling of the page

const style = `
  .nagBox {
    display: flex;
    align-items: center;
    gap: 1rem;
    padding: 1rem;
    margin-bottom: 2rem;
    box-sizing: border-box;
    border-style: double;
    border-width: 2px;
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
  .nagIcon {
    align-self: flex-start;
  }
  .recoveryNag {
    display: flex;
    flex-direction: column;
  }
  .recoveryNagTitle {
    font-weight: 600;
    font-size: 1.1rem;
  }
  .recoveryNagMessage {
    margin-top: 0.5rem;
    margin-bottom: 1rem;
    font-size: 1rem;
  }
  .recoveryNagButton {
    padding: 0.2rem 0.4rem;
    border-radius: 2px;
    width: fit-content;
    align-self: flex-end;
    margin: 0;
  }
  .labelWithAction {
    margin-top: 1rem;
    display: flex;
    justify-content: space-between;
  }

  .labelWithAction label {
    margin: 0;
  }

  .labelAction {
    padding: 0;
    border: none;
    display: inline;
    width: auto;
    margin: 0;
    cursor: pointer;
    color: #387ff7;
    font-size: 12px;
    font-family: "Montserrat", sans-serif;
    text-align: right;
    font-weight: 600;
  }
  .labelAction::before {
    content: "+";
    margin-right: 3px;
    color: #387ff7;
  }`;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const pageContent = (
  userNumber: bigint,
  devices: DeviceData[],
  connection: IIConnection
) => (
  <div>
    <style>{style}</style>
    <div className="container">
      <h1>Anchor Management</h1>
      <p>
        You can view and manage this Identity Anchor and its added devices here.
      </p>
      {hasRecoveryDevice(devices) && recoveryNag()}
      <label>Identity Anchor</label>
      <div className="highlightBox">{Number(userNumber) /* ... */}</div>
      <div className="labelWithAction">
        <label id="deviceLabel">Added devices</label>
        <button
          className="labelAction"
          id="addAdditionalDevice"
          onClick={() => addAdditionalDevice(userNumber, connection, devices)}
        >
          ADD NEW DEVICE
        </button>
      </div>
      <div id="deviceList"></div>
      {hasRecoveryDevice(devices) || (
        <div>
          <div className="labelWithAction">
            <label id="deviceLabel">Recovery mechanisms</label>
            <button className="labelAction" id="addRecovery">
              ADD RECOVERY MECHANISM
            </button>
          </div>
          <div id="recoveryList"></div>
        </div>
      )}
      {logoutSection()}
    </div>
    <div id="navbar">
      {/* TODO: add aboutLink */} &middot; {/* TODO: add faqLink */}
    </div>
  </div>
);

const recoveryNag = () => (
  <div className="nagBox">
    <div className="nagIcon">{/* TODO: add warningIcon */}</div>
    <div className="recoveryNag">
      <div className="recoveryNagTitle">Recovery Mechanism</div>
      <div className="recoveryNagMessage">
        Add a recovery mechanism to help protect this Identity Anchor.
      </div>
      <button
        id="addRecovery"
        className="primary recoveryNagButton"
        onClick={() => alert("Not supported :)")}
      >
        Add Recovery Key
      </button>
    </div>
  </div>
);

export const renderManage = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  document.title = "Manage | Internet Identity"; // Extra

  const container = document.getElementById("pageContent") as HTMLElement;
  let devices: DeviceData[];
  try {
    //devices = await withLoader(() => lookupAll(userNumber));
    devices = await lookupAll(userNumber);
  } catch (error) {
    await displayFailedToListDevices(error);
    return renderManage(userNumber, connection);
  }

  // NOTE: we cast userNumber to number because react fails to render big ints
  // https://github.com/facebook/react/issues/20492
  ReactDOM.render(pageContent(userNumber, devices, connection), container);
  // TODO: init
};

// Add a new device (i.e. a device connected to the device the user is
// currently using, like a YubiKey, or FaceID, or, or. Not meant to be used to
// add e.g. _another_ macbook or iPhone.)
const addAdditionalDevice = async (
  userNumber: bigint,
  connection: IIConnection,
  devices: DeviceData[]
) => {
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices),
    });
  } catch (error) {
    await displayFailedToAddNewDevice(error);
    return renderManage(userNumber, connection);
  }
  //const deviceName = await pickDeviceAlias();

  const deviceName = "my device";
  if (deviceName === null) {
    return renderManage(userNumber, connection);
  }
  try {
    //await withLoader(() =>
    //connection.add(
    //userNumber,
    //deviceName,
    //{ unknown: null },
    //{ authentication: null },
    //newDevice.getPublicKey().toDer(),
    //newDevice.rawId
    //)
    //);
    await connection.add(
      userNumber,
      deviceName,
      { unknown: null },
      { authentication: null },
      newDevice.getPublicKey().toDer(),
      newDevice.rawId
    );
  } catch (error) {
    await displayFailedToAddTheDevice(error);
  }
  renderManage(userNumber, connection);
};

// Whether or the user has registered a device as recovery
const hasRecoveryDevice = (devices: DeviceData[]): boolean =>
  !devices.some((device) => hasOwnProperty(device.purpose, "recovery"));

// TEMPORARY MOCKS

const lookupAll = async (userNumber: bigint) => {
  console.log(userNumber);
  return [];
};

const displayError = async ({}) => {};

const logoutSection = (alternativeLabel?: string) => (
  <div>
    <style>
      {`
    #logoutIcon {
      position: relative;
      top: 6px;
    }
    #logoutBox {
      margin-top: 3rem;
    }`}
    </style>
    <div id="logoutBox">
      <hr />
      <button type="button" className="linkStyle" id="logoutButton">
        {logoutIcon}
        {alternativeLabel !== undefined ? alternativeLabel : "Logout"}
      </button>
    </div>
  </div>
);

const logoutIcon = (
  <svg
    id="logoutIcon"
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M20.8195 17.7516H19.5586C19.4602 17.7516 19.3781 17.8336 19.3781 17.932V19.3805H4.61953V4.61953H19.3805V6.06797C19.3805 6.16641 19.4625 6.24844 19.5609 6.24844H20.8219C20.9203 6.24844 21.0023 6.16875 21.0023 6.06797V3.71953C21.0023 3.32109 20.6813 3 20.2828 3H3.71953C3.32109 3 3 3.32109 3 3.71953V20.2805C3 20.6789 3.32109 21 3.71953 21H20.2805C20.6789 21 21 20.6789 21 20.2805V17.932C21 17.8312 20.918 17.7516 20.8195 17.7516ZM21.2555 11.8523L17.9297 9.22734C17.8055 9.12891 17.625 9.21797 17.625 9.375V11.1562H10.2656C10.1625 11.1562 10.0781 11.2406 10.0781 11.3438V12.6562C10.0781 12.7594 10.1625 12.8438 10.2656 12.8438H17.625V14.625C17.625 14.782 17.8078 14.8711 17.9297 14.7727L21.2555 12.1477C21.2779 12.1301 21.296 12.1077 21.3085 12.0821C21.3209 12.0565 21.3274 12.0285 21.3274 12C21.3274 11.9715 21.3209 11.9435 21.3085 11.9179C21.296 11.8923 21.2779 11.8699 21.2555 11.8523Z"
      fill="#292A2E"
    />
  </svg>
);

export class IIConnection {
  public devices: DeviceData[] = [];

  add = async (
    userNumber: UserNumber,
    alias: string,
    keyType: KeyType,
    purpose: Purpose,
    newPublicKey: DerEncodedBlob,
    credentialId?: BinaryBlob
  ): Promise<void> => {
    console.log("New device added");
    // TODO
    //const actor = await this.getActor();
    //return await actor.add(userNumber, {
    //alias,
    //pubkey: Array.from(newPublicKey),
    //credential_id: credentialId ? [Array.from(credentialId)] : [],
    //key_type: keyType,
    //purpose,
    //});
  };
}
