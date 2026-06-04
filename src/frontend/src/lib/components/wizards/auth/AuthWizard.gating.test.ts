import { describe, it, expect } from "vitest";
import {
  resolveOpenIdAlreadyLinkedDispatcher,
  resolveOpenIdNotConnectedDispatcher,
} from "./AuthWizard.gating";

describe("AuthWizard gating — OpenID disambiguation dispatcher resolution", () => {
  const handler = () => undefined;

  describe("alreadyLinked", () => {
    it("dispatches in signup mode when result is signIn", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signup", handler),
      ).toBe(handler);
    });

    it("does NOT dispatch in both mode (only signup gates the dialog)", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "both", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch in signin mode", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signin", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch when the handler is absent", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signup", undefined),
      ).toBeUndefined();
    });

    it("does NOT dispatch when result is signUp", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signUp", "signup", handler),
      ).toBeUndefined();
    });
  });

  describe("notConnected", () => {
    it("dispatches in signin mode when result is signUp", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signin", handler),
      ).toBe(handler);
    });

    it("does NOT dispatch in both mode (only signin gates the dialog)", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "both", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch in signup mode", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signup", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch when the handler is absent", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signin", undefined),
      ).toBeUndefined();
    });

    it("does NOT dispatch when result is signIn", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signIn", "signin", handler),
      ).toBeUndefined();
    });
  });
});
