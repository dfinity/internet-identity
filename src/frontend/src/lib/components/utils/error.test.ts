import { beforeEach, describe, expect, it, vi } from "vitest";

const toasterMock = vi.hoisted(() => ({
  info: vi.fn(),
  warning: vi.fn(),
  error: vi.fn(),
}));

vi.mock("$lib/components/utils/toaster", () => ({ toaster: toasterMock }));
vi.mock("$lib/utils/analytics/authenticationV2Funnel", () => ({
  AuthenticationV2Events: {},
  authenticationV2Funnel: { trigger: vi.fn() },
}));
vi.mock("$lib/stores/locale.store", () => ({
  // Not preprocessed in unit tests, so templates arrive as written.
  t: (strings: TemplateStringsArray, ...parts: unknown[]) =>
    String.raw({ raw: strings }, ...parts),
}));

import { handleError } from "./error";
import { CallbackPopupClosedError, PopupBlockedError } from "$lib/utils/openID";

describe("handleError", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("says that a pop-up was blocked, not that the interaction was canceled", () => {
    handleError(new PopupBlockedError());

    expect(toasterMock.warning).toHaveBeenCalledTimes(1);
    expect(toasterMock.warning.mock.calls[0][0]).toMatchObject({
      title: "Pop-up blocked",
      description: expect.stringContaining("blocked the sign-in window"),
    });
    expect(toasterMock.info).not.toHaveBeenCalled();
    expect(toasterMock.error).not.toHaveBeenCalled();
  });

  it("says that the interaction was canceled when the pop-up was closed", () => {
    handleError(new CallbackPopupClosedError());

    expect(toasterMock.info).toHaveBeenCalledTimes(1);
    expect(toasterMock.info.mock.calls[0][0]).toMatchObject({
      title: "Operation canceled",
    });
    expect(toasterMock.warning).not.toHaveBeenCalled();
  });
});
