import { describe, expect, it, vi } from "vitest";
import { toaster } from "$lib/components/utils/toaster";
import { CanisterError } from "$lib/utils/utils";
import { showRepairFailure } from "./repairFailure";

vi.mock("$lib/components/utils/toaster", () => ({
  toaster: { error: vi.fn() },
}));
vi.mock("$lib/stores/locale.store", () => ({
  t: (strings: TemplateStringsArray) => strings.join(""),
}));

describe("showRepairFailure", () => {
  it("names a canister refusal by its variant", () => {
    showRepairFailure(new CanisterError({ NotReachable: null }));

    expect(vi.mocked(toaster.error)).toHaveBeenCalledWith(
      expect.objectContaining({ description: "NotReachable" }),
    );
  });
});
