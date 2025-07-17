import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import { Principal } from "@dfinity/principal";

import { idlFactory as vc_issuer_idl } from "./generated/vc_issuer_idl";
import { _SERVICE } from "./generated/vc_issuer_types";

/** A class for accessing the Issuer API */
export class VcIssuer {
  public constructor(readonly canisterId: string) {}

  createActor = async (): Promise<ActorSubclass<_SERVICE>> => {
    const agent = await HttpAgent.create({
      // XXX: we always fetch the rootkey.
      // Although this isn't necessary/recommended on mainnet,
      // we do this for simplicity.
      shouldFetchRootKey: true,
    });

    return Actor.createActor<_SERVICE>(vc_issuer_idl, {
      agent,
      canisterId: this.canisterId,
    });
  };

  addEmployee = async ({
    principal,
  }: {
    principal: string;
  }): Promise<string> => {
    const actor = await this.createActor();
    return await actor.add_employee(Principal.fromText(principal));
  };

  addAdult = async ({ principal }: { principal: string }): Promise<string> => {
    const actor = await this.createActor();
    return await actor.add_adult(Principal.fromText(principal));
  };
}
