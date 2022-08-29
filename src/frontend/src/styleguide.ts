/** A shocase of common CSS patterns that can be reuses all all over the app */
import "./styles/main.css";
import { html, render } from "lit-html";

export const styleguide = html`
  <style>
    .styleguide {
      margin: 10rem auto;
      max-width: 50rem;
      padding: 0 2rem;
    }
    .styleguide code {
      background-color: #202124;
      color: #fff;
      padding: 0.2em;
    }
    .demo {
      margin: 2rem 0;
      border: #202124 1px solid;
      padding: 1em;
      background-color: #f7f7f7;
      background-image: linear-gradient(45deg, #fff 25%, transparent 25%, transparent 75%, #fff 75%, #fff), linear-gradient(45deg, #fff 25%, transparent 25%, transparent 75%, #fff 75%, #fff);
      background-size: 3rem 3rem;
      background-position: 0 0, 1.5rem 1.5rem;
      border-radius: 0.5em;
    }
  </style>
  <section class="styleguide">
    <h1 class="t-title t-title--main">Design Patterns</h1>
    <article class="l-section">
      <h1 class="t-title">Typography</h1>
      <p class="t-lead">
        The font used all over the app is called "Montserrat" and is loaded from
        <a href="https://fonts.google.com/specimen/Montserrat">Google Fonts</a>. 
        It is a sans-serif font that is easy to read.
      </p>
      <p class="t-paragrapgh">
        The base font size is dependent on the browser's window size. But 1rem is
        our base unit. To make it easier to work with it, we consider 1rem beeing the 
        equivalent of ~10px.
      </p>
      <p class="t-paragrapgh">
        Since we use a CSS reset almost no html elements come with a default styling.
        To help style text element we have a bunch of utility classes that can be used.
      </p>
      <aside class="l-section">
        <h2 class="t-title t-title--sub">Tiltes</h2>
        <p class="t-lead">
          The <code>.t-title</code> class can be used to style titles. If you want to make it a
          main title, add the <code>.t-title--main</code> modifier class.
        </p>
        <section class="demo" aria-label="Titles Demo">
          <h1 class="t-title t-title--main">Large Title</h1>
          <h2 class="t-title">Default regular title</h2>
        </section>
      </aside>
      <aside class="l-section">
        <h2 class="t-title t-title--sub">Paragraphs</h2>
        <p class="t-lead">
          There are two types of text paragraphs. The <code>.t-lead</code> class 
          can be used to style lead paragraphs that follow a title. 
          The <code>.t-paragraph</code> class is a more generic class that can be used
          to style any other paragraph.
        </p>
        <section class="demo" aria-label="Paragraphs Demo">
          <p class="t-lead">This is a lead paragraph</p>
          <p class="t-paragraph">This is a regular paragraph</p>
        </section>
      </aside>
      <aside class="l-section">
        <h2 class="t-title t-title--sub">Various text helpers</h2>
        <p class="t-lead">
          There are a bunch of other text helpers that can be used to style text 
          within a paragraph or anywhere else. One of the rare elements that has a default style
          is a <a href="#">link</a> But sometimes you might want an other element
          to look like a link. In that case you can use the <code>.t-link</code> class.
          In some rare cases we don't want links to look less prominent (by removing the underline)
          you can use the <code>.t-link--discreet</code> class. 
        </p>
        
        <section class="demo" aria-label="Verious elements Demo">
          <p class="t-paragraph">
            <a href="#">This is an actual a tag</a><br />
            <button class="t-link">This is a button element that looks like a link</button><br />
            <button class="t-link t-link--discreet">This is a link that is not underlined</button><br />
            <a href="#" class="t-link"><i class="t-link__icon">+</i>Link with icon</a><br />
          </p>
        </section>
      </aside>
    </article>

    <article class="l-section">
      <h1 class="t-title">Layout</h1>
      <p class="t-lead">
        We use a bunch of layout classes to style the layout of the app. They just
        define where elements are placed.
      </p>
    </article>
  </section>
`;