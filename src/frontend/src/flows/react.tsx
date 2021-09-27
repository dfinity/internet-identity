import React, { useState, useEffect } from "react";
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

const AnchorManagement = (props: {
  userNumber: bigint;
  connection: IIConnection;
}) => {
  console.log("I am running.");

  const defaultAuthDevices: DeviceData[] = [];
  const defaultRecoveryDevices: DeviceData[] = [];

  const [authDevices, setAuthDevices] = useState(defaultAuthDevices);
  const [recoveryDevices, setRecoveryDevices] = useState(
    defaultRecoveryDevices
  );
  const allDevices = authDevices.concat(recoveryDevices);

  const removeDevice = (pubkey: PublicKey) => {
    props.connection.remove(pubkey);
  };

  async function fetchDevices() {
    let devices: DeviceData[];

    let newAuthDevices: DeviceData[] = [];
    let newRecoveryDevices: DeviceData[] = [];
    devices = await props.connection.lookupAll();
    for (var device of devices) {
      if (hasOwnProperty(device.purpose, "recovery")) {
        newRecoveryDevices.push(device);
      } else {
        newAuthDevices.push(device);
      }
    }

    setAuthDevices(newAuthDevices);
    setRecoveryDevices(newRecoveryDevices);
  }

  useEffect(() => {
    fetchDevices();
  }, []);

  return (
    <div>
      <style>{style}</style>
      <div className="container">
        <h1>Anchor Management</h1>
        <p>
          You can view and manage this Identity Anchor and its added devices
          here.
        </p>
        {!hasRecoveryDevice(recoveryDevices) || recoveryNag()}
        <label>Identity Anchor</label>
        <div className="highlightBox">
          {Number(props.userNumber) /* react struggles to render BigInts */}
        </div>
        <DeviceList
          devices={authDevices}
          deviceLabel="Added devices"
          labelAction="ADD NEW DEVICE"
          addAdditionalDevice={() => {
            addAdditionalDevice(props.userNumber, props.connection, allDevices);
            fetchDevices();
          }}
          removeDevice={(pubkey) => {
            removeDevice(pubkey);
            fetchDevices();
          }}
        ></DeviceList>
        {hasRecoveryDevice(allDevices) || (
          <DeviceList
            devices={authDevices}
            deviceLabel="Recovery mechanisms"
            labelAction="ADD RECOVERY MECHANISM"
            addAdditionalDevice={() =>
              alert("Adding recovery is not supported yet")
            }
            removeDevice={removeDevice}
          ></DeviceList>
        )}
        {logoutSection()}
      </div>
      <div id="navbar">
        {aboutLink} &middot; {faqLink}
      </div>
    </div>
  );
};

interface DeviceListProps {
  devices: DeviceData[];
  deviceLabel: string;
  labelAction: string;
  addAdditionalDevice: () => void;
  removeDevice: (arg0: PublicKey) => void;
}

function DeviceList(props: DeviceListProps) {
  return (
    <div>
      <div className="labelWithAction">
        <label>{props.deviceLabel}</label>
        <button className="labelAction" onClick={props.addAdditionalDevice}>
          {props.labelAction}
        </button>
      </div>
      {props.devices.length > 0 && (
        <ul>
          {props.devices.map((device) => (
            <li className="deviceItem" key={device.pubkey.toString()}>
              <div className="deviceItemAlias">{device.alias}</div>
              <button
                type="button"
                className="deviceItemRemove"
                onClick={() => props.removeDevice(device.pubkey)}
              >
                {closeIcon}
              </button>
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

const recoveryNag = () => (
  <div className="nagBox">
    <div className="nagIcon">{warningIcon}</div>
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

  // NOTE: we cast userNumber to number because react fails to render big ints
  // https://github.com/facebook/react/issues/20492
  ReactDOM.render(
    <AnchorManagement userNumber={userNumber} connection={connection} />,
    container
  );
};

// Add a new device (i.e. a device connected to the device the user is
// currently using, like a YubiKey, or FaceID, or, or. Not meant to be used to
// add e.g. _another_ macbook or iPhone.)
const addAdditionalDevice = async (
  userNumber: bigint,
  connection: IIConnection,
  devices: DeviceData[]
) => {
  const deviceName = "my device";
  await connection.add(deviceName, { authentication: null });
  console.log("done");
};

// Whether or the user has registered a device as recovery
const hasRecoveryDevice = (devices: DeviceData[]): boolean =>
  !devices.some((device) => hasOwnProperty(device.purpose, "recovery"));

// TEMPORARY MOCKS

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

const warningIcon = (
  <svg
    className="warningIcon"
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M22.3988 20.0625L12.6488 3.1875C12.5035 2.93672 12.2527 2.8125 11.9996 2.8125C11.7465 2.8125 11.4934 2.93672 11.3504 3.1875L1.6004 20.0625C1.31212 20.5641 1.67305 21.1875 2.24962 21.1875H21.7496C22.3262 21.1875 22.6871 20.5641 22.3988 20.0625ZM11.2496 9.75C11.2496 9.64687 11.334 9.5625 11.4371 9.5625H12.5621C12.6652 9.5625 12.7496 9.64687 12.7496 9.75V14.0625C12.7496 14.1656 12.6652 14.25 12.5621 14.25H11.4371C11.334 14.25 11.2496 14.1656 11.2496 14.0625V9.75ZM11.9996 18C11.7052 17.994 11.4249 17.8728 11.2188 17.6625C11.0128 17.4522 10.8973 17.1695 10.8973 16.875C10.8973 16.5805 11.0128 16.2978 11.2188 16.0875C11.4249 15.8772 11.7052 15.756 11.9996 15.75C12.294 15.756 12.5743 15.8772 12.7804 16.0875C12.9865 16.2978 13.1019 16.5805 13.1019 16.875C13.1019 17.1695 12.9865 17.4522 12.7804 17.6625C12.5743 17.8728 12.294 17.994 11.9996 18V18Z"
      fill="#292A2E"
    />
  </svg>
);

const newDevice = (alias: string, purpose: Purpose): DeviceData => {
  const items = ["iPhone", "MacBook Air", "XPS 13", "YubiKey", "iPad", "Dell"];
  const colors = [
    "black",
    "silver",
    "gray",
    "white",
    "maroon",
    "red",
    "purple",
    "fuchsia",
  ];
  const color = colors[Math.floor(Math.random() * colors.length)];
  const item = items[Math.floor(Math.random() * items.length)];

  return {
    alias: color + " " + item,
    pubkey: randomPubkey(),
    key_type: { unknown: null },
    purpose: purpose,
    credential_id: [],
  };
};

export class IIConnection {
  public devices: DeviceData[] = [];

  add = async (alias: string, purpose: Purpose): Promise<void> => {
    const newAlias = alias + `-${this.devices.length}`;
    this.devices.push(newDevice(newAlias, purpose));
  };

  lookupAll = async (): Promise<DeviceData[]> => {
    return this.devices;
  };

  remove = (publicKey: PublicKey) => {
    const newDevices = this.devices.filter(
      (device) => device.pubkey != publicKey
    );
    this.devices = newDevices;
  };
}

const randomPubkey = (): Array<number> => {
  return Array.from({ length: 40 }, () => Math.floor(Math.random() * 40));
};

const aboutLink = (
  <a id="aboutLink" className="navbar-link" href="/about" target="_blank">
    About
  </a>
);

export const faqLink = (
  <a id="faqLink" className="navbar-link" href="/faq" target="_blank">
    FAQ
  </a>
);

export const closeIcon = (
  <svg
    className="closeIcon"
    width="16"
    height="16"
    viewBox="0 0 16 16"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M8.8095 8L12.9111 3.11094C12.9798 3.02969 12.922 2.90625 12.8158 2.90625H11.5689C11.4954 2.90625 11.4251 2.93906 11.3767 2.99531L7.99388 7.02813L4.61106 2.99531C4.56419 2.93906 4.49388 2.90625 4.41888 2.90625H3.172C3.06575 2.90625 3.00794 3.02969 3.07669 3.11094L7.17825 8L3.07669 12.8891C3.06129 12.9072 3.05141 12.9293 3.04822 12.9529C3.04503 12.9764 3.04867 13.0004 3.05871 13.022C3.06874 13.0435 3.08475 13.0617 3.10483 13.0745C3.12492 13.0872 3.14823 13.0939 3.172 13.0938H4.41888C4.49231 13.0938 4.56263 13.0609 4.61106 13.0047L7.99388 8.97188L11.3767 13.0047C11.4236 13.0609 11.4939 13.0938 11.5689 13.0938H12.8158C12.922 13.0938 12.9798 12.9703 12.9111 12.8891L8.8095 8Z"
      fill="#262626"
    />
  </svg>
);
