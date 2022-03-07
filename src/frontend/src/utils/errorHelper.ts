/**
 * Toggles the CSS classes on error message element and the input element to either show or hide error information.
 * @param inputElementId id of the input element
 * @param errorMessageId id of the error message element
 * @param show Boolean flag whether the error information should be shown or not
 */
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
