import { mkAnchorPicker } from "$src/components/anchorPicker";
import { pinInput } from "$src/components/pinInput";
import { toast } from "$src/components/toast";
import { withRef } from "$src/utils/lit-html";
import { Chan, NonEmptyArray, asNonEmptyArray } from "$src/utils/utils";
import { TemplateResult, html, render } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";

export const componentsPage = () => {
  document.title = "Components";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(components(), container);
};

const components = (): TemplateResult => {
  return html`${mkToast()} ${choosePin()} ${pickAnchor()}`;
};

const mkToast = (): TemplateResult => {
  const toastMessage = html`This is a
    <strong class="t-strong">new</strong> message`;

  return html`<div class="c-card" style="max-width: 30em; margin: 40px auto;">
    <h2 class="t-title t-title--sub">Toasts</h2>
    <div class="c-button-group">
      <button
        @click=${() => toast.info(toastMessage)}
        class="c-button c-input--stack"
      >
        info
      </button>
      <button
        @click=${() => toast.success(toastMessage)}
        class="c-button c-input--stack"
      >
        success
      </button>
      <button
        @click=${() => toast.error(toastMessage)}
        class="c-button c-input--stack"
      >
        error
      </button>
    </div>
  </div>`;
};

const choosePin = (): TemplateResult => {
  const submittedPin = new Chan("N/A");
  const pinInput_ = pinInput({
    verify: (pin: string) => ({ ok: true, value: pin }),
    onSubmit: (pin) => submittedPin.send(pin),
  });

  return html` <div class="c-card" style="max-width: 30em; margin: 40px auto;">
    ${pinInput_.template}

    <output class="c-input c-input--readonly c-input--stack c-input--fullwidth">
      Submitted:
      <strong class="t-strong">${asyncReplace(submittedPin)}</strong>
    </output>
    <button @click=${() => pinInput_.submit()} class="c-button c-input--stack">
      submit
    </button>
  </div>`;
};

const pickAnchor = (): TemplateResult => {
  const showSelected: Chan<string> = new Chan("Please pick anchor");
  const savedAnchors: Ref<HTMLInputElement> = createRef();
  const updateSavedAnchors: Ref<HTMLButtonElement> = createRef();

  const mk = (anchors: NonEmptyArray<bigint>): TemplateResult =>
    mkAnchorPicker({
      savedAnchors: anchors,
      pick: (anchor: bigint) => showSelected.send(anchor.toString()),
      moreOptions: () => console.log("More options requested"),
      focus: false,
    }).template;

  const chan = new Chan<TemplateResult>(mk([BigInt(10055), BigInt(1669234)]));
  const update = () =>
    withRef(savedAnchors, (savedAnchors) => {
      const value = savedAnchors.value;
      if (value !== "") {
        const values = value.split(",").map((x) => BigInt(x));
        const anchors = asNonEmptyArray(values);
        if (anchors !== undefined) {
          chan.send(mk(anchors));
        }
      }
    });

  return html`
    <div class="c-card" style="margin: 40px;">
        <input class="c-input c-input--fullwidth" ${ref(
          savedAnchors
        )} placeholder="stored anchors: anchor1, anchor2, ..." ></input>
        <button class="c-button c-input--stack" ${ref(
          updateSavedAnchors
        )} @click="${update}">update</button>
        <div>${asyncReplace(chan)}</div>
        <div class="c-input c-input--stack c-input--readonly">${asyncReplace(
          showSelected
        )} </div>
    </div>`;
};
