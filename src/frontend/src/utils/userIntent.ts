// What does the user intend to do after logging in?
export type UserIntent = ManageIntent | AuthIntent | AddDeviceIntent;

export type ManageIntent = { kind: "manage" };
export type AuthIntent = { kind: "auth" };
export type AddDeviceIntent = { kind: "addDevice" };

export const intentFromUrl = (url: URL): UserIntent => {
  if (url.hash == "#authorize") {
    return { kind: "auth" };
  } else if (url.hash?.split("device=")[1]) {
    return { kind: "addDevice" };
  } else {
    return { kind: "manage" };
  }
};

export const verbFromIntent = (intent: UserIntent): string => {
  switch (intent.kind) {
    case "addDevice":
      return "to add your new device to your Internet Identity";
    case "auth":
      return "using your Internet Identity";
    case "manage":
      return "to manage your Internet Identity";
  }
};

export const verbFromIntent2 = (intent: UserIntent): string => {
  switch (intent.kind) {
    case "addDevice":
      return "and to add your new device to your Internet Identity";
    case "auth":
      return "using your Internet Identity";
    case "manage":
      return "and to manage your Internet Identity";
  }
};
