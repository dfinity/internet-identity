import { callbackFlow } from "$src/flows/redirect";
import { createSpa } from "./spa";

void createSpa(() => callbackFlow());
