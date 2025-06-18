import { E as current_component, F as noop, G as spread_props, D as pop, A as push, I as attr, J as attr_class, K as attr_style, M as clsx, N as stringify, O as spread_attributes, P as escape_html, Q as props_id, R as ensure_array_like } from "../../chunks/index.js";
import * as toast from "@zag-js/toast";
import { I as Icon, B as Button } from "../../chunks/Button.js";
import { isNullish, nonNullish } from "@dfinity/utils";
import "clsx";
import { createScope, MachineStatus, INIT_STATE } from "@zag-js/core";
import { identity, isFunction, compact, ensure, toArray, isString, warn } from "@zag-js/utils";
import { createNormalizer } from "@zag-js/types";
function onDestroy(fn) {
  var context = (
    /** @type {Component} */
    current_component
  );
  (context.d ??= []).push(fn);
}
const propMap = {
  className: "class",
  defaultChecked: "checked",
  defaultValue: "value",
  htmlFor: "for",
  onBlur: "onfocusout",
  onChange: "oninput",
  onFocus: "onfocusin",
  onDoubleClick: "ondblclick"
};
function toStyleString(style) {
  let string = "";
  for (let key in style) {
    const value = style[key];
    if (value === null || value === void 0)
      continue;
    if (!key.startsWith("--"))
      key = key.replace(/[A-Z]/g, (match) => `-${match.toLowerCase()}`);
    string += `${key}:${value};`;
  }
  return string;
}
const preserveKeys = "viewBox,className,preserveAspectRatio,fillRule,clipPath,clipRule,strokeWidth,strokeLinecap,strokeLinejoin,strokeDasharray,strokeDashoffset,strokeMiterlimit".split(",");
function toSvelteProp(key) {
  if (key in propMap)
    return propMap[key];
  if (preserveKeys.includes(key))
    return key;
  return key.toLowerCase();
}
function toSveltePropValue(key, value) {
  if (key === "style" && typeof value === "object")
    return toStyleString(value);
  return value;
}
const normalizeProps = createNormalizer((props) => {
  const normalized = {};
  for (const key in props) {
    normalized[toSvelteProp(key)] = toSveltePropValue(key, props[key]);
  }
  return normalized;
});
function bindable(props) {
  const initial = props().defaultValue ?? props().value;
  const eq = props().isEqual ?? Object.is;
  let value = initial;
  const controlled = props().value !== void 0;
  let valueRef = { current: value };
  let prevValue = { current: void 0 };
  const setValueFn = (v) => {
    const next = isFunction(v) ? v(valueRef.current) : v;
    const prev = prevValue.current;
    if (props().debug) {
      console.log(`[bindable > ${props().debug}] setValue`, { next, prev });
    }
    if (!controlled) value = next;
    if (!eq(next, prev)) {
      props().onChange?.(next, prev);
    }
  };
  function get() {
    return controlled ? props().value : value;
  }
  return {
    initial,
    ref: valueRef,
    get,
    set(val) {
      const exec = props().sync ? noop : identity;
      exec(() => setValueFn(val));
    },
    invoke(nextValue, prevValue2) {
      props().onChange?.(nextValue, prevValue2);
    },
    hash(value2) {
      return props().hash?.(value2) ?? String(value2);
    }
  };
}
bindable.cleanup = (fn) => {
  onDestroy(() => fn());
};
bindable.ref = (defaultValue) => {
  let value = defaultValue;
  return {
    get: () => value,
    set: (next) => {
      value = next;
    }
  };
};
function useRefs(refs) {
  const ref = { current: refs };
  return {
    get(key) {
      return ref.current[key];
    },
    set(key, value) {
      ref.current[key] = value;
    }
  };
}
const track = (deps, effect) => {
};
function access(userProps) {
  if (isFunction(userProps)) return userProps();
  return userProps;
}
function useMachine(machine, userProps) {
  const scope = (() => {
    const { id, ids, getRootNode } = access(userProps);
    return createScope({ id, ids, getRootNode });
  })();
  const debug = (...args) => {
    if (machine.debug) console.log(...args);
  };
  const props = machine.props?.({
    props: compact(access(userProps)),
    scope
  }) ?? access(userProps);
  const prop = useProp(() => props);
  const context = machine.context?.({
    prop,
    bindable,
    get scope() {
      return scope;
    },
    flush,
    getContext() {
      return ctx;
    },
    getComputed() {
      return computed;
    },
    getRefs() {
      return refs;
    }
  });
  const ctx = {
    get(key) {
      return context?.[key].get();
    },
    set(key, value) {
      context?.[key].set(value);
    },
    initial(key) {
      return context?.[key].initial;
    },
    hash(key) {
      const current = context?.[key].get();
      return context?.[key].hash(current);
    }
  };
  let effects = /* @__PURE__ */ new Map();
  let transitionRef = { current: null };
  let previousEventRef = { current: null };
  let eventRef = { current: { type: "" } };
  const getEvent = () => ({
    ...eventRef.current,
    current() {
      return eventRef.current;
    },
    previous() {
      return previousEventRef.current;
    }
  });
  const getState = () => ({
    ...state,
    hasTag(tag) {
      const currentState = state.get();
      return !!machine.states[currentState]?.tags?.includes(tag);
    },
    matches(...values) {
      const currentState = state.get();
      return values.includes(currentState);
    }
  });
  const refs = useRefs(machine.refs?.({ prop, context: ctx }) ?? {});
  const getParams = () => ({
    state: getState(),
    context: ctx,
    event: getEvent(),
    prop,
    send,
    action,
    guard,
    track,
    refs,
    computed,
    flush,
    scope,
    choose
  });
  const action = (keys) => {
    const strs = isFunction(keys) ? keys(getParams()) : keys;
    if (!strs) return;
    const fns = strs.map((s) => {
      const fn = machine.implementations?.actions?.[s];
      if (!fn) warn(`[zag-js] No implementation found for action "${JSON.stringify(s)}"`);
      return fn;
    });
    for (const fn of fns) {
      fn?.(getParams());
    }
  };
  const guard = (str) => {
    if (isFunction(str)) return str(getParams());
    return machine.implementations?.guards?.[str](getParams());
  };
  const effect = (keys) => {
    const strs = isFunction(keys) ? keys(getParams()) : keys;
    if (!strs) return;
    const fns = strs.map((s) => {
      const fn = machine.implementations?.effects?.[s];
      if (!fn) warn(`[zag-js] No implementation found for effect "${JSON.stringify(s)}"`);
      return fn;
    });
    const cleanups = [];
    for (const fn of fns) {
      const cleanup = fn?.(getParams());
      if (cleanup) cleanups.push(cleanup);
    }
    return () => cleanups.forEach((fn) => fn?.());
  };
  const choose = (transitions) => {
    return toArray(transitions).find((t) => {
      let result = !t.guard;
      if (isString(t.guard)) result = !!guard(t.guard);
      else if (isFunction(t.guard)) result = t.guard(getParams());
      return result;
    });
  };
  const computed = (key) => {
    ensure(machine.computed, () => `[zag-js] No computed object found on machine`);
    const fn = machine.computed[key];
    return fn({
      context: ctx,
      event: getEvent(),
      prop,
      refs,
      scope,
      computed
    });
  };
  const state = bindable(() => ({
    defaultValue: machine.initialState({ prop }),
    onChange(nextState, prevState) {
      if (prevState) {
        const exitEffects = effects.get(prevState);
        exitEffects?.();
        effects.delete(prevState);
      }
      if (prevState) {
        action(machine.states[prevState]?.exit);
      }
      action(transitionRef.current?.actions);
      const cleanup = effect(machine.states[nextState]?.effects);
      if (cleanup) effects.set(nextState, cleanup);
      if (prevState === INIT_STATE) {
        action(machine.entry);
        const cleanup2 = effect(machine.effects);
        if (cleanup2) effects.set(INIT_STATE, cleanup2);
      }
      action(machine.states[nextState]?.entry);
    }
  }));
  let status = MachineStatus.NotStarted;
  onDestroy(() => {
    debug("unmounting...");
    status = MachineStatus.Stopped;
    effects.forEach((fn) => fn?.());
    effects = /* @__PURE__ */ new Map();
    transitionRef.current = null;
    action(machine.exit);
  });
  const send = (event) => {
    if (status !== MachineStatus.Started) return;
    previousEventRef.current = eventRef.current;
    eventRef.current = event;
    let currentState = state.get();
    const transitions = machine.states[currentState].on?.[event.type] ?? machine.on?.[event.type];
    const transition = choose(transitions);
    if (!transition) return;
    transitionRef.current = transition;
    const target = transition.target ?? currentState;
    const changed = target !== currentState;
    if (changed) {
      state.set(target);
    } else if (transition.reenter && !changed) {
      state.invoke(currentState, currentState);
    } else {
      action(transition.actions);
    }
  };
  machine.watch?.(getParams());
  return {
    get state() {
      return getState();
    },
    send,
    context: ctx,
    prop,
    get scope() {
      return scope;
    },
    refs,
    computed,
    get event() {
      return getEvent();
    },
    getStatus: () => status
  };
}
function useProp(value) {
  return function get(key) {
    return value()[key];
  };
}
function flush(fn) {
}
function Circle_alert($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    [
      "circle",
      { "cx": "12", "cy": "12", "r": "10" }
    ],
    [
      "line",
      {
        "x1": "12",
        "x2": "12",
        "y1": "8",
        "y2": "12"
      }
    ],
    [
      "line",
      {
        "x1": "12",
        "x2": "12.01",
        "y1": "16",
        "y2": "16"
      }
    ]
  ];
  Icon($$payload, spread_props([
    { name: "circle-alert" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
function Circle_check($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    [
      "circle",
      { "cx": "12", "cy": "12", "r": "10" }
    ],
    ["path", { "d": "m9 12 2 2 4-4" }]
  ];
  Icon($$payload, spread_props([
    { name: "circle-check" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
function Info($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    [
      "circle",
      { "cx": "12", "cy": "12", "r": "10" }
    ],
    ["path", { "d": "M12 16v-4" }],
    ["path", { "d": "M12 8h.01" }]
  ];
  Icon($$payload, spread_props([
    { name: "info" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
function X($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    ["path", { "d": "M18 6 6 18" }],
    ["path", { "d": "m6 6 12 12" }]
  ];
  Icon($$payload, spread_props([
    { name: "x" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
function ProgressRing($$payload, $$props) {
  push();
  const { value, class: className } = $$props;
  const strokeWidth = 2;
  let clientWidth = 0;
  const circumference = 2 * Math.PI * ((clientWidth - strokeWidth) * 0.5);
  $$payload.out += `<svg${attr("viewBox", `0 0 ${stringify(clientWidth)} ${stringify(clientWidth)}`)}${attr_class(
    clsx([
      "size-5 -rotate-90 rounded-full",
      className,
      isNullish(value) && "progress_rotate"
    ]),
    "svelte-1c2mug8"
  )}${attr_style(`--circumference: ${stringify(circumference)}px`)}><circle${attr_class(clsx(["fill-none stroke-current/20"]), "svelte-1c2mug8")}${attr("stroke-width", strokeWidth)}${attr("r", (clientWidth - strokeWidth) / 2)}${attr("cx", clientWidth / 2)}${attr("cy", clientWidth / 2)}></circle><circle${attr_class(
    clsx([
      "fill-none stroke-current",
      isNullish(value) && "progress_pulsate"
    ]),
    "svelte-1c2mug8"
  )} stroke-linecap="round"${attr("stroke-width", strokeWidth)}${attr("r", (clientWidth - strokeWidth) / 2)}${attr("cx", clientWidth / 2)}${attr("cy", clientWidth / 2)}${attr("stroke-dasharray", circumference)}${attr("stroke-dashoffset", circumference - circumference / 100 * (value ?? 0))}></circle></svg>`;
  pop();
}
function Alert($$payload, $$props) {
  push();
  const {
    children,
    class: className,
    title,
    description,
    variant = "info",
    direction = "vertical",
    onClose,
    $$slots,
    $$events,
    ...props
  } = $$props;
  $$payload.out += `<div${spread_attributes(
    {
      ...props,
      class: clsx([
        "bg-bg-primary_alt border-border-tertiary grid grid-cols-[auto_1fr_auto] grid-rows-[auto_auto] items-start gap-4 rounded-xl border p-4",
        className
      ])
    },
    null
  )}><div${attr_class(clsx([direction === "horizontal" && "row-span-2"]))}>`;
  if (variant === "info") {
    $$payload.out += "<!--[-->";
    Info($$payload, {
      size: "1.25rem",
      class: "text-fg-brand-primary"
    });
  } else if (variant === "success") {
    $$payload.out += "<!--[1-->";
    Circle_check($$payload, {
      size: "1.25rem",
      class: "text-fg-success-primary"
    });
  } else if (variant === "warning") {
    $$payload.out += "<!--[2-->";
    Circle_alert($$payload, {
      size: "1.25rem",
      class: "text-fg-warning-primary"
    });
  } else if (variant === "error") {
    $$payload.out += "<!--[3-->";
    Circle_alert($$payload, {
      size: "1.25rem",
      class: "text-fg-error-primary"
    });
  } else if (variant === "loading") {
    $$payload.out += "<!--[4-->";
    ProgressRing($$payload, { class: "text-fg-brand-primary" });
  } else {
    $$payload.out += "<!--[!-->";
  }
  $$payload.out += `<!--]--></div> `;
  if (nonNullish(onClose)) {
    $$payload.out += "<!--[-->";
    Button($$payload, {
      onclick: onClose,
      variant: "tertiary",
      size: "sm",
      iconOnly: true,
      class: [
        "col-start-3 -m-2 !rounded-full",
        direction === "horizontal" && "row-span-2"
      ],
      children: ($$payload2) => {
        X($$payload2, { size: "1.25rem" });
      },
      $$slots: { default: true }
    });
  } else {
    $$payload.out += "<!--[!-->";
  }
  $$payload.out += `<!--]--> <div${attr_class(clsx([
    "flex flex-col gap-1",
    direction === "horizontal" ? "col-start-2 row-span-2 row-start-1" : "col-span-3",
    direction === "horizontal" && isNullish(onClose) && "col-span-2"
  ]))}><div class="text-text-primary text-sm font-semibold">${escape_html(title)}</div> `;
  if (nonNullish(description) || nonNullish(children)) {
    $$payload.out += "<!--[-->";
    $$payload.out += `<div class="flex flex-col gap-3">`;
    if (nonNullish(description)) {
      $$payload.out += "<!--[-->";
      $$payload.out += `<div class="text-text-tertiary text-sm font-medium">${escape_html(description)}</div>`;
    } else {
      $$payload.out += "<!--[!-->";
    }
    $$payload.out += `<!--]--> `;
    if (nonNullish(children)) {
      $$payload.out += "<!--[-->";
      $$payload.out += `<div class="flex gap-3">`;
      children($$payload);
      $$payload.out += `<!----></div>`;
    } else {
      $$payload.out += "<!--[!-->";
    }
    $$payload.out += `<!--]--></div>`;
  } else {
    $$payload.out += "<!--[!-->";
  }
  $$payload.out += `<!--]--></div></div>`;
  pop();
}
function Toast($$payload, $$props) {
  push();
  const { toast: toastProps, index, parent } = $$props;
  const machineProps = { ...toastProps, parent, index };
  const service = useMachine(toast.machine, () => machineProps);
  const api = toast.connect(service, normalizeProps);
  const variant = api.type;
  let innerWidth = window.innerWidth;
  const mobile = innerWidth < 480;
  $$payload.out += `<div${spread_attributes({ ...api.getRootProps() }, "svelte-1po8ll6")}>`;
  Alert($$payload, {
    title: api.title,
    description: api.description,
    variant,
    onClose: api.closable ? api.dismiss : void 0,
    direction: mobile ? "vertical" : "horizontal",
    class: [
      "shadow-lg not-dark:bg-clip-padding max-sm:w-[calc(100vw_-_2rem)] sm:w-100",
      "!border-black/[0.08]",
      "dark:!border-surface-dark-700"
    ]
  });
  $$payload.out += `<!----></div>`;
  pop();
}
const toaster = toast.createStore({
  placement: "top-end",
  overlap: false
});
function Toaster($$payload, $$props) {
  push();
  const id = props_id($$payload);
  const service = useMachine(toast.group.machine, { id, store: toaster });
  const api = toast.group.connect(service, normalizeProps);
  const each_array = ensure_array_like(api.getToasts());
  $$payload.out += `<div${spread_attributes({ ...api.getGroupProps() }, null)}><!--[-->`;
  for (let index = 0, $$length = each_array.length; index < $$length; index++) {
    let toast2 = each_array[index];
    Toast($$payload, { toast: toast2, index, parent: service });
  }
  $$payload.out += `<!--]--></div>`;
  pop();
}
function Logo($$payload, $$props) {
  const { $$slots, $$events, ...props } = $$props;
  $$payload.out += `<svg${spread_attributes({ viewBox: "0 0 46 22", ...props }, null, void 0, void 0, 3)}><path d="M45.9992 11C45.9992 4.93463 40.9792 0 34.8188 0C32.2505 0 29.4539 1.33375 26.4911 3.96324C25.0903 5.2064 23.8757 6.5361 22.9637 7.60537C19.7613 3.99235 15.4664 0 11.1804 0C5.99986 0 1.4822 3.63324 0.306661 8.44735C0.308259 8.44331 0.309856 8.43765 0.311453 8.43199C0.309856 8.43765 0.308259 8.4425 0.306661 8.44735C0.106213 9.2675 0 10.1224 0 11C0 17.0654 4.94012 22 11.1005 22C13.6688 22 16.5453 20.6663 19.5081 18.0368C20.9089 16.7936 22.1235 15.4639 23.0355 14.3946C26.2379 18.0076 30.5336 22 34.8196 22C40.0001 22 44.5178 18.3668 45.6933 13.5535C45.8938 12.7333 46 11.8784 46 11.0008L45.9992 11ZM23.823 7.59404C24.8699 6.39779 25.9081 5.34147 26.9127 4.45015C29.7557 1.92662 32.4159 0.647059 34.8188 0.647059C40.6318 0.647059 45.3603 5.29132 45.3603 11C45.3603 11.8007 45.2653 12.6007 45.0768 13.3788C45.0465 13.4685 44.6592 14.5426 43.5555 15.5868C42.122 16.9432 40.179 17.6315 37.78 17.6324C40.3603 16.4968 42.1651 13.9457 42.1651 11C42.1651 6.99551 38.8693 3.73838 34.8188 3.73838C33.24 3.73838 31.3074 4.74375 29.0721 6.72779C28.0667 7.62074 27.0485 8.67059 25.9736 9.92507L25.5519 10.4152L23.3917 8.07529L23.823 7.59485V7.59404ZM20.0017 11.1084C19.1488 12.134 17.9189 13.5316 16.5054 14.7861C13.8716 17.1244 12.1594 17.6146 11.1804 17.6146C7.48605 17.6146 4.47294 14.647 4.47294 11C4.47294 7.35301 7.48286 4.40809 11.1804 4.38544C11.3145 4.38544 11.4766 4.39919 11.6731 4.43478C13.5785 5.17485 15.2804 6.35574 16.3082 7.30853C17.1347 8.07529 18.6321 9.64441 20.0009 11.1076L20.0017 11.1084ZM22.1762 14.406C21.1285 15.6022 20.0911 16.6585 19.0865 17.5499C16.2834 20.0378 13.5218 21.3529 11.1005 21.3529C8.29343 21.3529 5.66126 20.2732 3.68712 18.3126C1.72098 16.3601 0.638878 13.7629 0.638878 11C0.638878 10.1993 0.73471 9.39853 0.92238 8.62125C0.954324 8.52824 1.34164 7.45574 2.44371 6.41316C3.87719 5.05677 5.82018 4.36846 8.21916 4.36765C5.6389 5.50324 3.83407 8.05427 3.83407 11C3.83407 15.0045 7.12988 18.2616 11.1804 18.2616C12.7592 18.2616 14.6918 17.2563 16.9271 15.2722C17.9325 14.3793 18.9507 13.3294 20.0256 12.0749L20.4465 11.584C20.4465 11.584 22.5747 13.8891 22.5979 13.915L22.1754 14.406H22.1762ZM25.9975 10.8916C26.8504 9.86603 28.0803 8.46838 29.4938 7.2139C32.1276 4.87559 33.8398 4.38544 34.8188 4.38544C38.5131 4.38544 41.5263 7.35301 41.5263 11C41.5263 14.647 38.5163 17.5919 34.8188 17.6146C34.6847 17.6146 34.5226 17.6008 34.3253 17.5652C34.3269 17.5652 34.3277 17.566 34.3293 17.5668C32.4222 16.8268 30.7188 15.6451 29.6902 14.6915C28.8637 13.9247 27.3663 12.3556 25.9967 10.8924L25.9975 10.8916ZM45.6885 13.5648C45.6901 13.5607 45.6909 13.5559 45.6925 13.5526C45.6917 13.5559 45.6901 13.5607 45.6885 13.5648Z" class="fill-current"></path></svg>`;
}
function Header($$payload, $$props) {
  const {
    children,
    class: className,
    $$slots,
    $$events,
    ...props
  } = $$props;
  $$payload.out += `<header${spread_attributes(
    {
      ...props,
      class: clsx([
        "flex h-14 items-center px-4 py-3 md:px-6 lg:px-8",
        className
      ])
    },
    null
  )}><div class="flex h-14 items-center gap-4">`;
  Logo($$payload, { class: "text-fg-primary h-5.5" });
  $$payload.out += `<!----> <h1 class="text-md text-text-primary font-semibold md:block">Internet Identity</h1></div></header>`;
}
const SUPPORT_URL = "https://identitysupport.dfinity.org";
function Footer($$payload, $$props) {
  const {
    children,
    class: className,
    $$slots,
    $$events,
    ...props
  } = $$props;
  $$payload.out += `<footer${spread_attributes(
    {
      ...props,
      class: clsx([
        "flex h-14 items-center justify-between px-4 py-3 md:px-6 lg:px-8",
        className
      ])
    },
    null
  )}><div class="text-text-tertiary text-sm font-medium">© Internet Identity</div> <nav class="text-text-primary flex gap-4 text-sm font-semibold"><a${attr("href", SUPPORT_URL)} target="_blank" rel="noopener">Support</a></nav></footer>`;
}
function _layout($$payload, $$props) {
  const {
    children,
    class: className,
    $$slots,
    $$events,
    ...rest
  } = $$props;
  $$payload.out += `<div${spread_attributes(
    {
      class: clsx([
        "flex min-h-[100dvh] w-screen flex-col",
        className
      ]),
      ...rest
    },
    null
  )}><div class="h-[env(safe-area-inset-top)]"></div> `;
  Header($$payload, {});
  $$payload.out += `<!----> <div class="flex flex-1 flex-col items-center justify-center">`;
  children?.($$payload);
  $$payload.out += `<!----></div> `;
  Footer($$payload, {});
  $$payload.out += `<!----> <div class="h-[env(safe-area-inset-bottom)]"></div></div> `;
  Toaster($$payload);
  $$payload.out += `<!---->`;
}
export {
  _layout as default
};
