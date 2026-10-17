/** Empty where its record is gone and where it never had one, which the app answers the same way.
 * The caller brings the actor, so this holds nothing of the document's and loads in a worker where there is no `document`. */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

export const appAnchors = async (
  actor: ActorSubclass<_SERVICE>,
): Promise<bigint[]> =>
  // Candid's `vec nat64` binds as a typed array or a plain one, and a typed array's `map` answers another typed array.
  Array.from(await actor.notification_app_anchors());
