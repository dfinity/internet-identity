import oauth from "../utils/oath";
import { renderIndex } from "../pages";
import { renderManage } from "../pages/manage";
import { init } from ".dfx/local/canisters/idp_service/idp_service.did";

export const navigateTo = (route: string) => {
  renderApp();
  if (history.length > initialHistoryLength) {
    history.replaceState({}, "Internet Identity", route);
  } else {
    history.pushState({}, "Internet Identity", route);
  }
};

const renderApp = () => {
  if (window.location.href.match(/authorize/)) {
    oauth();
    renderIndex();
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
