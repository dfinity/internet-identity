// Copied verbatim (modulo linting comment, formatting & eager importing) from NNS dapp
// https://github.com/dfinity/nns-dapp/blob/4619857a316be5b47a950c6b8461ffcaa1a3dde3/frontend/src/lib/utils/html.utils.ts#L94
//

import { isNullish } from "@dfinity/utils";
import type { marked as markedTypes, Renderer } from "marked";
import { marked } from "marked";

type Marked = typeof markedTypes;

export const targetBlankLinkRenderer = (
  href: string | null | undefined,
  title: string | null | undefined,
  text: string,
): string =>
  `<a${
    href === null || href === undefined
      ? ""
      : ` target="_blank" rel="noopener noreferrer" href="${href}"`
  }${title === null || title === undefined ? "" : ` title="${title}"`}>${
    text.length === 0 ? (href ?? title) : text
  }</a>`;

/**
 * Based on https://github.com/markedjs/marked/blob/master/src/Renderer.js#L186
 * @returns <a> tag to image
 */
export const imageToLinkRenderer = (
  src: string | null | undefined,
  title: string | null | undefined,
  alt: string,
): string => {
  if (src === undefined || src === null || src?.length === 0) {
    return alt;
  }
  const fileExtention = src.includes(".")
    ? (src.split(".").pop() as string)
    : "";
  // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#attr-type
  const typeProp =
    fileExtention === "" ? undefined : ` type="image/${fileExtention}"`;
  const titleDefined = title !== undefined && title !== null;
  const titleProp = titleDefined ? ` title="${title}"` : undefined;
  const text = alt === "" ? (titleDefined ? title : src) : alt;

  return `<a href="${src}" target="_blank" rel="noopener noreferrer"${
    typeProp ?? ""
  }${titleProp ?? ""}>${text}</a>`;
};

const escapeHtml = (html: string): string =>
  html.replace(/</g, "&lt;").replace(/>/g, "&gt;");
const escapeSvgs = (html: string): string =>
  html.replace(/<svg[^>]*>[\s\S]*?<\/svg>/gi, escapeHtml);

/**
 * Escape <img> tags or convert them to links
 */
const transformImg = (img: string): string => {
  const src = img.match(/src="([^"]+)"/)?.[1];
  // eslint-disable-next-line
  const alt = img.match(/alt="([^"]+)"/)?.[1] || "img";
  const title = img.match(/title="([^"]+)"/)?.[1];
  const shouldEscape = isNullish(src) || src.startsWith("data:image");
  const imageHtml = shouldEscape
    ? escapeHtml(img)
    : imageToLinkRenderer(src, title, alt);

  return imageHtml;
};

/** Avoid <img> tags; instead, apply the same logic as for markdown images by either escaping them or converting them to links. */
export const htmlRenderer = (html: string): string =>
  /<img\s+[^>]*>/gi.test(html) ? transformImg(html) : html;

/**
 * Marked.js renderer for proposal summary.
 * Customized renderers
 * - targetBlankLinkRenderer
 * - imageToLinkRenderer
 * - htmlRenderer
 *
 * @param marked
 */
const proposalSummaryRenderer = (marked: Marked): Renderer => {
  const renderer = new marked.Renderer();

  renderer.link = targetBlankLinkRenderer;
  renderer.image = imageToLinkRenderer;
  renderer.html = htmlRenderer;

  return renderer;
};

/**
 * Uses markedjs.
 * Escape or transform to links some raw HTML tags (img, svg)
 * @see {@link https://github.com/markedjs/marked}
 */
export const markdownToHTML = async (text: string): Promise<string> => {
  // Replace the SVG elements in the HTML with their escaped versions to improve security.
  // It's not possible to do it with html renderer because the svg consists of multiple tags.
  // One edge case is not covered: if the svg is inside the <code> tag, it will be rendered as with &lt; & &gt; instead of "<" & ">"
  const escapedText = escapeSvgs(text);

  return await marked(escapedText, {
    renderer: proposalSummaryRenderer(marked),
  });
};
