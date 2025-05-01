import { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { Account, AccountNumber } from "$lib/generated/internet_identity_types";

export type State =
  | { state: "loading"; label?: string }
  | {
      state: "continueAs";
      currentIdentity: LastUsedIdentity;
      identities: LastUsedIdentity[];
      continueAs: (identity: LastUsedIdentity) => void;
      switchIdentity: (identity: LastUsedIdentity) => void;
      useAnotherIdentity: () => void;
      useAnotherAccount: (identity: LastUsedIdentity) => void;
    }
  | {
      state: "pickAccount";
      currentAccountNumber: AccountNumber | undefined;
      accounts: Account[];
      selectAccount: (accountNumber: AccountNumber | undefined) => void;
      createAccount: (name: string) => void;
      authenticate: () => void;
    }
  | {
      state: "pickAuthenticationMethod";
    }
  | {
      state: "connectOrCreatePasskey";
      connect: () => void;
      create: () => void;
    }
  | {
      state: "createPasskey";
      create: (name: string) => void;
      cancel: () => void;
    }
  | {
      state: "solveCaptcha";
      image: string;
      attempt: number;
      solve: (solution: string) => void;
      cancel: () => void;
    };
