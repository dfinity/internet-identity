import oauth from "../utils/oath";
import { renderIndex } from "../pages";
import { renderManage } from "../pages/manage";

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
