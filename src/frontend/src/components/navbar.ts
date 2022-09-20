import { html } from "lit";
import { navigationLink } from "../components/navigationLink";

export const navbar = html` <aside aria-label="General links">
  <ul class="c-nav-links">
    <li>
      ${navigationLink({
        labelText: "About",
        uuid: "aboutLink",
        url: "/about",
      })}
    </li>
    <li>
      ${navigationLink({
        labelText: "FAQ",
        uuid: "faqLink",
        url: "/faq",
      })}
    </li>
  </ul>
</aside>`;
