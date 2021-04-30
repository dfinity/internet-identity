// What does the user intend to do after logging in?
export type UserIntent = ManageIntent | AuthIntent | OAuthIntent | AddDeviceIntent

export type ManageIntent = { kind: "manage" }
export type AuthIntent = { kind: "auth" }
export type OAuthIntent = { kind: "oauth" }
export type AddDeviceIntent = { kind: "addDevice" }

export const intentFromUrl = (url: URL): UserIntent => {
    if (url.hash == "#authorize") {
      return { kind: "auth" }
    } else if (url.href.match(/authorize/)) {
      return { kind: "oauth" }
    } else if (!!url.hash?.split("device=")[1]) {
      return { kind: "addDevice" }
    } else {
      return { kind: "manage" }
    }
}

export const bannerFromIntent = (intent: UserIntent): string => {
    switch (intent.kind) {
      case "addDevice": return "add a new device to your Internet Identity"
      case "auth": return "authenticate using your Internet Identity"
      case "oauth": return "authenticate using your Internet Identity"
      case "manage": return "manage your Internet Identity"
    }
  }
