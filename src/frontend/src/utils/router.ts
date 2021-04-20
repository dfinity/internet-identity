import oauth from "../utils/oath";
import { renderIndex } from "../pages";
import { renderManage } from "../pages/manage";
import { getUserId } from "./userId";
import { IDPActor } from "./idp_actor";

export const navigateTo = (route: string) => {
  if (history.length > initialHistoryLength) {
    history.replaceState({}, "Internet Identity", route);
  } else {
    history.pushState({}, "Internet Identity", route);
  }
  renderApp();
};

const renderApp = () => {
  if (window.location.href.match(/authorize/)) {
    const userId = getUserId();
    if (userId === undefined) {
      renderIndex()
    } else {
      IDPActor.reconnect(userId).then(connection =>
        oauth(userId, connection)
      )
    }
  } else if (window.location.href.match(/manage/)) {
    renderManage();
  } else {
    renderIndex();
  }
};

const initialHistoryLength = history.length;

export const routerInit = () => {
  window.onpopstate = renderApp;
  renderApp();
};
