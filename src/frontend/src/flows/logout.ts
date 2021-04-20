import { confirm } from "../components/confirm";
import idp_actor from "../utils/idp_actor";

export const initLogout = () => {
  const logoutButton = document.querySelector(
    "#logout-button"
  ) as HTMLButtonElement;

  logoutButton.onclick = handleLogout;
};

const handleLogout = async (e) => {
  e.preventDefault();
  const secondaryMessage = idp_actor.userId
    ? `Your userId is ${idp_actor.userId}. Please make sure you have saved this information so that you can login again. If you are ready to logout, click "Confirm".`
    : undefined;
  try {
    if (await confirm("Are you sure you want to logout?", secondaryMessage)) {
      localStorage.clear();
      location.reload();
    }
  } catch (error) {}
};
