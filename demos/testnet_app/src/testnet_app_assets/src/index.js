import { Actor, HttpAgent } from '@dfinity/agent';
import { DelegationIdentity } from "@dfinity/identity";
import { AuthClient } from "@dfinity/auth-client";
import { Principal } from "@dfinity/principal";
import { idlFactory as testnet_app_idl, canisterId as testnet_app_id } from 'dfx-generated/testnet_app';


// const testnetII = "https://rdmx6-jaaaa-aaaaa-aaadq-cai.identity.dfinity.network/"
const testnetII = "localhost:8081/"

document.getElementById("loginBtn").addEventListener("click", async () => {
  const authClient = await AuthClient.create();
  await authClient.login({
    identityProvider: testnetII,
    maxTimeToLive: BigInt(maxTimeToLive.value)
  })
  const identity = authClient.getIdentity()

  console.log({identity})

  document.getElementById("loginStatus").innerText = identity.getPrincipal().toText()

  const agent = new HttpAgent();
  const testnet_app = Actor.createActor(testnet_app_idl, { agent, canisterId: testnet_app_id, identity });
  const whomAmI = await testnet_app.whoami();
  console.log({whomAmI})

});