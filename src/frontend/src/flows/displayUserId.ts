import { html, render } from "lit-html";

const pageContent = (userId) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>You're now registered. Keep your User ID safe, it is your responsibility.</p>
    <p>Memorize this phrase. Save a backup in multiple places. Without this code, you can not recover your internet identity.</p>
    <div class="userIdBox">
      <label>User ID:</label>
      ${userId}
    </div>
    <button id="displayUserContinue" class="primary">Continue</button>
  </div>
  `;

export const displayUserId = async (userId: bigint): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userId), container);
  return init()
}

const init = (): Promise<void> => new Promise(resolve => {
  const displayUserContinue = document.getElementById("displayUserContinue") as HTMLButtonElement;
  displayUserContinue.onclick = () => resolve()
});
