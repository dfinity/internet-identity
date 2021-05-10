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

export const bannerFromIntent = (intent: UserIntent): string => {
  switch (intent.kind) {
    case "addDevice":
      return "add your new device to your Internet Identity";
    case "auth":
      return "authenticate using your Internet Identity";
    case "manage":
      return "manage your Internet Identity";
  }
};
