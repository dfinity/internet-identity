import { render, html } from "lit-html";

export const flavors = {
    FETCH_ROOT_KEY: process.env.II_FETCH_ROOT_KEY === "1",
    DUMMY_AUTH: process.env.II_DUMMY_AUTH === "1",
    DUMMY_CAPTCHA: process.env.II_DUMMY_CAPTCHA === "1",
};

export const anyFlavors = () => {
    return Object.values(flavors).indexOf(true) >= 0;
}

export const showWarningOnFlavors = () => {
    if(anyFlavors()) {
        showWarning();
    }
}

export const showWarning = () => {
    let container = document.createElement("div");

    let warning = html`
    <div
        class="flavors-warning"
        style="background: red; width: 100vw; position: fixed; top: 0; padding: 0 1em;"
        >
        This Internet Identity is insecure because it was built with flavors
        <a style="display: inline-block; margin: 0 1em;" href="https://github.com/dfinity/internet-identity#build-flavors">more</a>
        <a style="display: inline-block; margin: 0 1em;" href="#" onclick=${() => { container.remove(); }}">close</a>
    </div>
    `;

    render(warning, container);

    document.body.appendChild(container);
};
