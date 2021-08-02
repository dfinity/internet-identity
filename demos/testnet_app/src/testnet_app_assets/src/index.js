import { Actor, HttpAgent } from '@dfinity/agent';
import { DelegationIdentity } from "@dfinity/identity";
import { AuthClient } from "@dfinity/auth-client";
import { Principal } from "@dfinity/principal";
import { idlFactory as testnet_app_idl, canisterId as testnet_app_id } from 'dfx-generated/testnet_app';


const testnetII = "https://rdmx6-jaaaa-aaaaa-aaadq-cai.identity.dfinity.network/"
// const testnetII =  "http://localhost:8080/"

document.getElementById("loginBtn").addEventListener("click", async () => {
  const authClient = await AuthClient.create();
  // This API is just weird
  await new Promise((resolve, reject) => {
    authClient.login({
    identityProvider: testnetII,
    onSuccess: resolve,
    onError: reject
    })
  })
  const identity = authClient.getIdentity()

  console.log({identity})

  const agent = new HttpAgent({ identity });
  const testnet_app = Actor.createActor(testnet_app_idl, { agent, canisterId: testnet_app_id });
  const whoAmI = await testnet_app.whoami();
  console.log({whoAmI})

  document.getElementById("loginStatus").innerText = whoAmI.toText()

});