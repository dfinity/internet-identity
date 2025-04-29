export type State =
  | { state: "loading"; label?: string }
  | {
      state: "continueAs";
      number: bigint;
      name?: string;
      continue: () => void;
      useAnother: () => void;
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
