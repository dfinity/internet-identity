export type State =
  | { state: "continueAs"; number: bigint; name?: string }
  | {
      state: "authenticate";
      pick: (method: "passkey" | "google") => void;
      cancel: () => void;
      step: AuthenticateStep;
    };

export type AuthenticateStep =
  | {
      step: "pickAuthenticationMethod";
    }
  | {
      step: "connectOrCreatePasskey";
      connect: () => void;
      create: () => void;
    }
  | {
      step: "createPasskey";
      create: (name: string) => void;
      back: () => void;
    }
  | {
      step: "solveCaptcha";
      image: string;
      attempt: number;
      solve: (solution: string) => void;
      cancel: () => void;
    };
