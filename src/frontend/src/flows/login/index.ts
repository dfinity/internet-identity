import { Connection } from "../../utils/iiConnection";
import { LoginData } from "./flowResult";
import {
  authenticateBox,
  AuthTemplates,
} from "../../components/authenticateBox";
import { html } from "lit-html";

/* Template for the authbox when authenticating to II */
const tpls: AuthTemplates = {
  message: html`<p class="t-lead">Authenticate to manage your anchor</p>`,
};

/* the II authentication flow */
export const auth = async (connection: Connection): Promise<LoginData> =>
  authenticateBox(connection, tpls);
