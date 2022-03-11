// What does the user intend to do after logging in?
export type UserIntent = ManageIntent | AuthIntent;

export type ManageIntent = { kind: "manage" };
export type AuthIntent = { kind: "auth" };

export const intentFromUrl = (url: URL): UserIntent => {
  if (url.hash == "#authorize") {
    return { kind: "auth" };
  } else {
    return { kind: "manage" };
  }
};
