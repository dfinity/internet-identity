import { R as ensure_array_like, O as spread_attributes, M as clsx, W as element, D as pop, A as push, X as bind_props, Y as copy_payload, Z as assign_payload, G as spread_props } from "./index.js";
import { nonNullish } from "@dfinity/utils";
/**
 * @license @lucide/svelte v0.513.0 - ISC
 *
 * ISC License
 * 
 * Copyright (c) for portions of Lucide are held by Cole Bemis 2013-2022 as part of Feather (MIT). All other copyright (c) for Lucide are held by Lucide Contributors 2022.
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 */
const defaultAttributes = {
  xmlns: "http://www.w3.org/2000/svg",
  width: 24,
  height: 24,
  viewBox: "0 0 24 24",
  fill: "none",
  stroke: "currentColor",
  "stroke-width": 2,
  "stroke-linecap": "round",
  "stroke-linejoin": "round"
};
function Icon($$payload, $$props) {
  push();
  const {
    name,
    color = "currentColor",
    size = 24,
    strokeWidth = 2,
    absoluteStrokeWidth = false,
    iconNode = [],
    children,
    $$slots,
    $$events,
    ...props
  } = $$props;
  const each_array = ensure_array_like(iconNode);
  $$payload.out += `<svg${spread_attributes(
    {
      ...defaultAttributes,
      ...props,
      width: size,
      height: size,
      stroke: color,
      "stroke-width": absoluteStrokeWidth ? Number(strokeWidth) * 24 / Number(size) : strokeWidth,
      class: clsx([
        "lucide-icon lucide",
        name && `lucide-${name}`,
        props.class
      ])
    },
    null,
    void 0,
    void 0,
    3
  )}><!--[-->`;
  for (let $$index = 0, $$length = each_array.length; $$index < $$length; $$index++) {
    let [tag, attrs] = each_array[$$index];
    element($$payload, tag, () => {
      $$payload.out += `${spread_attributes({ ...attrs }, null, void 0, void 0, 3)}`;
    });
  }
  $$payload.out += `<!--]-->`;
  children?.($$payload);
  $$payload.out += `<!----></svg>`;
  pop();
}
function ButtonOrAnchor($$payload, $$props) {
  push();
  let {
    element: element2 = void 0,
    $$slots,
    $$events,
    ...props
  } = $$props;
  const buttonProps = !("href" in props) ? props : void 0;
  const anchorProps = "href" in props ? props : void 0;
  if (nonNullish(buttonProps)) {
    $$payload.out += "<!--[-->";
    $$payload.out += `<button${spread_attributes({ ...buttonProps }, null)}>`;
    buttonProps.children?.($$payload);
    $$payload.out += `<!----></button>`;
  } else if (nonNullish(anchorProps)) {
    $$payload.out += "<!--[1-->";
    $$payload.out += `<a${spread_attributes({ ...anchorProps }, null)}>`;
    anchorProps.children?.($$payload);
    $$payload.out += `<!----></a>`;
  } else {
    $$payload.out += "<!--[!-->";
  }
  $$payload.out += `<!--]-->`;
  bind_props($$props, { element: element2 });
  pop();
}
function Button($$payload, $$props) {
  push();
  let {
    children,
    element: element2 = void 0,
    class: className,
    variant = "primary",
    size = "md",
    iconOnly,
    $$slots,
    $$events,
    ...props
  } = $$props;
  let $$settled = true;
  let $$inner_payload;
  function $$render_inner($$payload2) {
    ButtonOrAnchor($$payload2, spread_props([
      props,
      {
        class: [
          "box-border flex items-center justify-center justify-self-start rounded-md font-semibold whitespace-nowrap opacity-100",
          {
            primary: [
              "bg-bg-brand-solid text-text-primary-inversed",
              "not-disabled:hover:bg-bg-brand-solid_hover",
              "disabled:bg-bg-disabled disabled:text-fg-disabled"
            ],
            secondary: [
              "bg-bg-primary border-border-secondary text-fg-primary border",
              "not-disabled:hover:bg-bg-primary_hover",
              "disabled:border-border-disabled disabled:text-fg-disabled"
            ],
            tertiary: [
              "text-fg-primary",
              "not-disabled:hover:bg-bg-primary_hover",
              "disabled:text-fg-disabled"
            ]
          }[variant],
          "focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary outline-none focus-visible:ring-2 focus-visible:ring-offset-2",
          {
            sm: iconOnly ? "size-9" : "h-9 gap-1.5 px-3 text-sm",
            md: iconOnly ? "size-10" : "h-10 gap-1.5 px-3.5 text-sm",
            lg: iconOnly ? "size-11" : "text-md h-11 gap-2.5 px-4",
            xl: iconOnly ? "size-12" : "text-md h-12 gap-2.5 px-4.5"
          }[size],
          className
        ],
        get element() {
          return element2;
        },
        set element($$value) {
          element2 = $$value;
          $$settled = false;
        },
        children: ($$payload3) => {
          children?.($$payload3);
          $$payload3.out += `<!---->`;
        },
        $$slots: { default: true }
      }
    ]));
  }
  do {
    $$settled = true;
    $$inner_payload = copy_payload($$payload);
    $$render_inner($$inner_payload);
  } while (!$$settled);
  assign_payload($$payload, $$inner_payload);
  bind_props($$props, { element: element2 });
  pop();
}
export {
  Button as B,
  Icon as I,
  ButtonOrAnchor as a
};
