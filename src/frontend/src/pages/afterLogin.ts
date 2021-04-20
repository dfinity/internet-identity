import { IDPActor } from "../utils/idp_actor";
import oauth from "../utils/oath";
import { addDevice } from "./addDevice";
import { renderManage } from "./manageNew";

export const afterLogin = (userId: bigint, connection: IDPActor) => {
    const url = new URL(document.URL)
    if (window.location.href.match(/authorize/)) {
        oauth(userId, connection)
      } else if (!!url.hash?.split("device=")[1]) {
        addDevice(userId, connection);
      } else {
        renderManage(userId, connection);
      }
}