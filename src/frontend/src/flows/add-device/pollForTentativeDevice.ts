import {html, render} from "lit-html";
import {IIConnection} from "../../utils/iiConnection";
import {renderManage} from "../manage";
import {withLoader} from "../../components/loader";

const pageContent = () => html`
  <div class="container">
    <h1>Add New Remote Device</h1>
    <p>
      Device registration process started. You have 15 min to complete the process.
      Please follow these steps to add your new device:
    </p>
    <ol>
      <li>Open <b>https://identity.ic0.app</b> in a browser on your new device</li>
      <li>Chose the "Already have an anchor but using a new device?" option</li>
      <li>Follow the instructions displayed on that page</li>
    </ol>
    <button id="cancelAddRemoteDevice" class="linkStyle">Cancel</button>
  </div>
`;

export const pollForTentativeDevice = async (userNumber: bigint, connection: IIConnection): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  await withLoader(async () => {
    const timestamp = await connection.enableDeviceRegistrationMode(userNumber);
    console.log("end registration mode: " + timestamp);
  })
  init(userNumber, connection);
};

function startPolling(connection: IIConnection, userNumber: bigint): number {
  const pollingHandle = window.setInterval(async () => {
    const userInfo = await connection.lookupAnchorInfo(userNumber);
    if (userInfo.tentative_device.length === 1) {
      const tentative_device = userInfo.tentative_device[0];
      window.clearInterval(pollingHandle);
      console.log("clearing interval, tentative device: " + tentative_device);
    }
  }, 2000);
  return pollingHandle;
}

const init = (userNumber: bigint, connection: IIConnection) => {
  const pollingHandle = startPolling(connection, userNumber);

  const displayUserContinue = document.getElementById(
    "cancelAddRemoteDevice"
  ) as HTMLButtonElement;
  displayUserContinue.onclick = async () => {
    window.clearInterval(pollingHandle);
    console.log("clearing interval due to cancel");
    await renderManage(userNumber, connection);
  };
};
