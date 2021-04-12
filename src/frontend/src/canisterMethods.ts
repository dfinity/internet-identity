import { Actor, HttpAgent, Principal } from "@dfinity/agent";
import IDPService, {
  // @ts-ignore
  idlFactory as idp_service_idl,
  // @ts-ignore
  canisterId as idp_service_id,
} from "dfx-generated/idp_service";

const agent = new HttpAgent();
const idp_service = Actor.createActor(idp_service_idl, {
  agent,
  canisterId: idp_service_id,
}) as IDPService;
const env = process.env["NODE_ENV"];

export async function register(
  userId: string,
  publicKey: number[],
  credentialId?: number[]
) {
  return idp_service.register(userId, publicKey, credentialId);
}

export async function add(
  userId: string,
  publicKey: number[],
  credentialId?: number[]
) {
  return idp_service.add(userId, publicKey, credentialId);
}

export async function remove(userId: string, publicKey: number[]) {
  return idp_service.remove(userId, publicKey);
}

export async function lookup(userId: string) {
  return idp_service.lookup(userId);
}

export default {
  register,
  add,
  remove,
  lookup,
};
