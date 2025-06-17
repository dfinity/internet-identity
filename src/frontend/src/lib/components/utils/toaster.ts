import * as toast from "@zag-js/toast";

export const toaster = toast.createStore({
  placement: "top-end",
  overlap: false,
  offsets: { top: "4rem", left: "1rem", bottom: "1rem", right: "1rem" },
});
