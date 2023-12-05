import { vcFlow } from "./flows/verifiableCredentials";
import { createSpa } from "./spa";

void createSpa((connection) => vcFlow({ connection }));
