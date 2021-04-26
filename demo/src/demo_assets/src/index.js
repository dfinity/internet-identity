import { Actor, HttpAgent } from '@dfinity/agent';
import { idlFactory as demo_idl, canisterId as demo_id } from 'dfx-generated/demo';

const agent = new HttpAgent();
const demo = Actor.createActor(demo_idl, { agent, canisterId: demo_id });

document.getElementById("loginBtn").addEventListener("click", async () => {
  const whoami = await demo.whoami();

  document.getElementById("result").innerText = whoami;
});
