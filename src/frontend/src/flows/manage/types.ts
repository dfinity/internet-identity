import { TemplateResult } from "lit-html";

// A simple authenticator (non-recovery device)
export type Authenticator = {
  alias: string;
  rename: () => void;
  remove?: () => void;
  warn?: TemplateResult;
};

// A recovery phrase, potentially protected
export type RecoveryPhrase = {
  reset: () => void;
} & Protection;

export type Protection =
  | { isProtected: true; unprotect: () => void }
  | { isProtected: false; protect: () => void };

// A recovery key, i.e. "external hardware"
export type RecoveryKey = {
  remove: () => void;
};

// The devices an anchor is expected to have
export type Devices = {
  authenticators: Authenticator[];
  recoveries: {
    recoveryKey?: RecoveryKey;
    recoveryPhrase?: RecoveryPhrase;
  };
};
