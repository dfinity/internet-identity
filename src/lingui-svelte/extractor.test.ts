import { svelteExtractor } from "./extractor";
import { describe, expect } from "vitest";
import { ExtractedMessage } from "@lingui/conf";

const FILE_NAME = "test.svelte";

const extract = (code: string): Promise<ExtractedMessage> =>
  new Promise<ExtractedMessage>((resolve, reject) => {
    try {
      svelteExtractor.extract(FILE_NAME, code, resolve);
    } catch (err) {
      reject(err);
    }
  });

describe("svelteExtractor", () => {
  describe("tagged template", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract("<span>{$t`Hello World`}</span>");
      expect(origin).toEqual([FILE_NAME, 1, 9]);
    });

    it.each([
      {
        case: "without variables",
        code: "<span>{$t`Hello World`}</span>",
        expected: "Hello World",
      },
      {
        case: "with named variable",
        code: "<span>{$t`Hello {name}`}</span>",
        expected: "Hello {name}",
      },
      {
        case: "with positional variable",
        code: '<span>{$t`Hello ${"John"}`}</span>',
        expected: "Hello {0}",
      },
      {
        case: "with named and positional variables",
        code: '<span>{$t`Hello {name}, ${"John"}, {friend} and ${"Jack"}`}</span>',
        expected: "Hello {name}, {0}, {friend} and {1}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("call expression", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract(
        '<span>{$t({ message: "Hello World" })}</span>',
      );
      expect(origin).toEqual([FILE_NAME, 1, 7]);
    });

    it("should extract context", async () => {
      const { context } = await extract(
        '<span>{$t({ message: "Hello World", context: "Greeting the world" })}</span>',
      );
      expect(context).toEqual("Greeting the world");
    });

    it.each([
      {
        case: "without variables",
        code: '<span>{$t({ message: "Hello World" })}</span>',
        expected: "Hello World",
      },
      {
        case: "with named variable",
        code: "<span>{$t({ message: `Hello ${name}` })}</span>",
        expected: "Hello {name}",
      },
      {
        case: "with positional variable",
        code: '<span>{$t({ message: `Hello ${"John"}` })}</span>',
        expected: "Hello {0}",
      },
      {
        case: "with named and positional variables",
        code: '<span>{$t({ message: `Hello {name}, ${"John"}, {friend} and ${"Jack"}` })}</span>',
        expected: "Hello {name}, {0}, {friend} and {1}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });

  describe("plural call expression", () => {
    it("should extract filename, line and column number", async () => {
      const { origin } = await extract(
        '<span>{$plural(1, { one: "One book", other: "# Books" })}</span>',
      );
      expect(origin).toEqual([FILE_NAME, 1, 7]);
    });

    it.each([
      {
        case: "with num value",
        code: '<span>{$plural(1, { one: "One book", other: "# Books" })}</span>',
        expected: "{num, plural, one {One book} other {# Books}}",
      },
      {
        case: "with num variable",
        code: '<span>{$plural(numBooks, { one: "One book", other: "# Books" })}</span>',
        expected: "{numBooks, plural, one {One book} other {# Books}}",
      },
      {
        case: "with exact plural",
        code: '<span>{$plural(0, { one: "One book", other: "# Books", "=0": "No books" })}</span>',
        expected: "{num, plural, one {One book} other {# Books} =0 {No books}}",
      },
      {
        case: "with named variable",
        code: '<span>{$plural(1, { one: "One {genre} book", other: "# {genre} books" })}</span>',
        expected:
          "{num, plural, one {One {genre} book} other {# {genre} books}}",
      },
      {
        case: "with positional variable",
        code: '<span>{$plural(1, { one: `One ${"fantasy"} book`, other: `# ${"fantasy"} books` })}</span>',
        expected: "{num, plural, one {One {0} book} other {# {0} books}}",
      },
      {
        case: "with named and positional variables",
        code: '<span>{$plural(1, { one: `One {genre} and ${"fantasy"} book`, other: `# {genre} and ${"fantasy"} books` })}</span>',
        expected:
          "{num, plural, one {One {genre} and {0} book} other {# {genre} and {0} books}}",
      },
    ])("should extract message $case", async ({ code, expected }) => {
      const { message } = await extract(code);
      expect(message).toEqual(expected);
    });
  });
});
