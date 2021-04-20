import { confirm } from "../components/confirm";
import { getUserId } from "../utils/userId";

export const initLogout = () => {
  const logoutButton = document.querySelector(
    "#logout-button"
  ) as HTMLButtonElement;

  logoutButton.onclick = handleLogout;
};

const handleLogout = async (e) => {
  e.preventDefault();
  const secondaryMessage = getUserId()
    ? `Your userId is ${getUserId()}. Please make sure you have saved this information so that you can login again. If you are ready to logout, click "Confirm".`
    : undefined;
  try {
    if (await confirm("Are you sure you want to logout?", secondaryMessage)) {
      localStorage.clear();
      location.reload();
    }
  } catch (error) {
    console.error(error);
  }
};
