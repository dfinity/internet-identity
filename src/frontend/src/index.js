import { Actor, HttpAgent } from '@dfinity/agent';
import { idlFactory as idp_service_idl, canisterId as idp_service_id } from 'dfx-generated/idp_service';

const agent = new HttpAgent();
const idp_service = Actor.createActor(idp_service_idl, { agent, canisterId: idp_service_id });

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  const greeting = await idp_service.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
