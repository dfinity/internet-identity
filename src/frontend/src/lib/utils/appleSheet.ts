/**
 * How long after the window gets its focus back a completed Sign in with Apple
 * sheet starts leaving the page, which tells it apart from a cancel.
 */
const LEAVE_AFTER_FOCUS_MILLIS = 500;

/**
 * How long after the window gets its focus back the page may still be here
 * when the sheet completed: Apple's reply replaces it within moments.
 */
const REPLACED_AFTER_FOCUS_MILLIS = 5000;

/**
 * Calls `onCancel` once when the user cancels Apple's native Sign in with
 * Apple sheet, on a page that is navigating to Apple behind it.
 *
 * Canceling the sheet can drop the navigation without telling the page: the
 * window only gets its focus back. A completed sheet gives focus back too, but
 * the navigation then goes on and `beforeunload` follows within milliseconds,
 * so focus that stays without one is a cancel.
 *
 * A cancel can also let the navigation go on towards Apple's web page, which
 * fires `beforeunload` as well. A completed sheet replaces the page with
 * Apple's reply within moments, so a page still here seconds after the sheet
 * closed is a cancel either way.
 *
 * iOS tells the page nothing when the sheet is canceled, so pages behind the
 * sheet also offer a way out (see `AppleConnectingView`).
 * @param onCancel Called at most once, when the sheet was canceled.
 */
export const callWhenAppleSheetIsCanceled = (onCancel: () => void): void => {
  let isSheetOpened = false;
  let isLeaving = false;
  let isCanceled = false;
  const cancelOnce = () => {
    if (isCanceled) {
      return;
    }
    isCanceled = true;
    onCancel();
  };
  window.addEventListener("blur", () => (isSheetOpened = true));
  window.addEventListener("beforeunload", () => (isLeaving = true));
  window.addEventListener("focus", () => {
    if (!isSheetOpened) {
      return;
    }
    setTimeout(() => {
      if (isLeaving) {
        return;
      }
      cancelOnce();
    }, LEAVE_AFTER_FOCUS_MILLIS);
    setTimeout(cancelOnce, REPLACED_AFTER_FOCUS_MILLIS);
  });
};
