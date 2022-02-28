export function toggleErrorMessage(
  inputElementId: string,
  errorMessageId: string,
  show: boolean
): void {
  const errorMessage = document.getElementById(errorMessageId) as HTMLElement;
  errorMessage.classList.toggle("error-message-hidden", !show);
  errorMessage.classList.toggle("error-message", show);
  const input = document.getElementById(inputElementId) as HTMLElement;
  input.classList.toggle("errored", show);
}
