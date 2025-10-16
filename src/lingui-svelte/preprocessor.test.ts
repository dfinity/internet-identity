import { describe, expect } from "vitest";
import { svelteTransform } from "./preprocessor";

describe("sveltePreprocessor", () => {
  describe("tagged template", () => {
    it.each([
      {
        case: "without variables",
        source: "{$t`Hello World`}",
        build: '{$t({ id: "mY42CM" })}',
        dev: '{$t({ id: "mY42CM", message: "Hello World" })}',
      },
      {
        case: "with named variable",
        source: "{$t`Hello ${name}`}",
        build: '{$t({ id: "OVaF9k", values: { name } })}',
        dev: '{$t({ id: "OVaF9k", message: "Hello {name}", values: { name } })}',
      },
      {
        case: "with positional variable",
        source: '{$t`Hello ${"John"}`}',
        build: '{$t({ id: "Y7riaK", values: { 0: "John" } })}',
        dev: '{$t({ id: "Y7riaK", message: "Hello {0}", values: { 0: "John" } })}',
      },
      {
        case: "with named and positional variables",
        source: '{$t`Hello ${name}, ${"John"}, ${friend} and ${"Jack"}`}',
        build:
          '{$t({ id: "EvFFeo", values: { 0: "John", 1: "Jack", name, friend } })}',
        dev: '{$t({ id: "EvFFeo", message: "Hello {name}, {0}, {friend} and {1}", values: { 0: "John", 1: "Jack", name, friend } })}',
      },
    ])("should transform code $case", ({ source, build, dev }) => {
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });
  });

  describe("call expression", () => {
    it("should retain explicit id", () => {
      const source = '{$t({ id: "HELLO_WORLD", message: "Hello World" })}';
      const build = '{$t({ id: "HELLO_WORLD" })}';
      const dev = '{$t({ id: "HELLO_WORLD", message: "Hello World" })}';
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });

    it("should omit optional context", () => {
      const source =
        '{$t({ message: "Hello World", context: "Greeting the world" })}';
      const build = '{$t({ id: "8MlzJp" })}';
      const dev = '{$t({ id: "8MlzJp", message: "Hello World" })}';
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });

    it.each([
      {
        case: "without variables",
        source: '{$t({ message: "Hello World" })}',
        build: '{$t({ id: "mY42CM" })}',
        dev: '{$t({ id: "mY42CM", message: "Hello World" })}',
      },
      {
        case: "with named variable",
        source: "{$t({ message: `Hello ${name}` })}",
        build: '{$t({ id: "OVaF9k", values: { name } })}',
        dev: '{$t({ id: "OVaF9k", message: "Hello {name}", values: { name } })}',
      },
      {
        case: "with positional variable",
        source: '{$t({ message: `Hello ${"John"}` })}',
        build: '{$t({ id: "Y7riaK", values: { 0: "John" } })}',
        dev: '{$t({ id: "Y7riaK", message: "Hello {0}", values: { 0: "John" } })}',
      },
      {
        case: "with named and positional variables",
        source:
          '{$t({ message: `Hello ${name}, ${"John"}, ${friend} and ${"Jack"}` })}',
        build:
          '{$t({ id: "EvFFeo", values: { 0: "John", 1: "Jack", name, friend } })}',
        dev: '{$t({ id: "EvFFeo", message: "Hello {name}, {0}, {friend} and {1}", values: { 0: "John", 1: "Jack", name, friend } })}',
      },
    ])("should transform code $case", ({ source, build, dev }) => {
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });
  });

  describe("plural call expression", () => {
    it.each([
      {
        case: "with num value",
        source: '{$plural(1, { one: "One book", other: "# Books" })}',
        build: '{$plural({ id: "MJ+EYH", values: { num: 1 } })}',
        dev: '{$plural({ id: "MJ+EYH", message: "{num, plural, one {One book} other {# Books}}", values: { num: 1 } })}',
      },
      {
        case: "with num variable",
        source: '{$plural(numBooks, { one: "One book", other: "# Books" })}',
        build: '{$plural({ id: "lUlJTW", values: { numBooks } })}',
        dev: '{$plural({ id: "lUlJTW", message: "{numBooks, plural, one {One book} other {# Books}}", values: { numBooks } })}',
      },
      {
        case: "with exact plural",
        source:
          '{$plural(0, { one: "One book", other: "# Books", "=0": "No books" })}',
        build: '{$plural({ id: "iNT1za", values: { num: 0 } })}',
        dev: '{$plural({ id: "iNT1za", message: "{num, plural, one {One book} other {# Books} =0 {No books}}", values: { num: 0 } })}',
      },
      {
        case: "with named variable",
        source:
          "{$plural(1, { one: `One ${genre} book`, other: `# ${genre} books` })}",
        build: '{$plural({ id: "KLyifH", values: { num: 1, genre } })}',
        dev: '{$plural({ id: "KLyifH", message: "{num, plural, one {One {genre} book} other {# {genre} books}}", values: { num: 1, genre } })}',
      },
      {
        case: "with positional variable",
        source:
          '{$plural(1, { one: `One ${"fantasy"} book`, other: `# ${"fantasy"} books` })}',
        build: '{$plural({ id: "ZCUu5z", values: { 0: "fantasy", num: 1 } })}',
        dev: '{$plural({ id: "ZCUu5z", message: "{num, plural, one {One {0} book} other {# {0} books}}", values: { 0: "fantasy", num: 1 } })}',
      },
      {
        case: "with named and positional variables",
        source:
          '{$plural(1, { one: `One ${genre} and ${"fantasy"} book`, other: `# ${genre} and ${"fantasy"} books` })}',
        build:
          '{$plural({ id: "T8f71C", values: { 0: "fantasy", num: 1, genre } })}',
        dev: '{$plural({ id: "T8f71C", message: "{num, plural, one {One {genre} and {0} book} other {# {genre} and {0} books}}", values: { 0: "fantasy", num: 1, genre } })}',
      },
    ])("should transform code $case", ({ source, build, dev }) => {
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });
  });

  describe("<Trans> component", () => {
    it("should retain explicit id", () => {
      const source = '<Trans id="HELLO_WORLD">Hello world</Trans>';
      const build = '<Trans id="HELLO_WORLD"></Trans>';
      const dev = '<Trans id="HELLO_WORLD" message={"Hello world"}></Trans>';
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });

    it("should omit optional context", () => {
      const source = '<Trans context="Greeting the world">Hello world</Trans>';
      const build = '<Trans id="Yp3/Os"></Trans>';
      const dev = '<Trans id="Yp3/Os" message={"Hello world"}></Trans>';
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });

    it.each([
      {
        case: "without variables",
        source: "<Trans>Hello world</Trans>",
        build: '<Trans id="1nGWAC"></Trans>',
        dev: '<Trans id="1nGWAC" message={"Hello world"}></Trans>',
      },
      {
        case: "with named variable",
        source: "<Trans>Hello {name}</Trans>",
        build: '<Trans id="OVaF9k" values={{ name }}></Trans>',
        dev: '<Trans id="OVaF9k" message={"Hello {name}"} values={{ name }}></Trans>',
      },
      {
        case: "with positional variable",
        source: '<Trans>Hello {"John"}</Trans>',
        build: '<Trans id="Y7riaK" values={{ 0: "John" }}></Trans>',
        dev: '<Trans id="Y7riaK" message={"Hello {0}"} values={{ 0: "John" }}></Trans>',
      },
      {
        case: "with named and positional variables",
        source: '<Trans>Hello {name}, {"John"}, {friend} and {"Jack"}</Trans>',
        build:
          '<Trans id="EvFFeo" values={{ 0: "John", 1: "Jack", name, friend }}></Trans>',
        dev: '<Trans id="EvFFeo" message={"Hello {name}, {0}, {friend} and {1}"} values={{ 0: "John", 1: "Jack", name, friend }}></Trans>',
      },
      {
        case: "with tag",
        source: '<Trans>Click <a href="/upgrade">here</a> to upgrade</Trans>',
        build:
          '<Trans id="RUiQ7+">' +
          "{#snippet renderNode(__children, __index)}" +
          '{#if __index === 0}<a href="/upgrade">{@render __children()}</a>{/if}' +
          "{/snippet}" +
          "</Trans>",
        dev:
          '<Trans id="RUiQ7+" message={"Click <0>here</0> to upgrade"}>' +
          "{#snippet renderNode(__children, __index)}" +
          '{#if __index === 0}<a href="/upgrade">{@render __children()}</a>{/if}' +
          "{/snippet}" +
          "</Trans>",
      },
      {
        case: "with self closing tag",
        source: "<Trans>Hello<br>World</Trans>",
        build:
          '<Trans id="yajsv9">' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<br>{/if}" +
          "{/snippet}" +
          "</Trans>",
        dev:
          '<Trans id="yajsv9" message={"Hello<0/>World"}>' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<br>{/if}" +
          "{/snippet}" +
          "</Trans>",
      },
      {
        case: "with nested tags",
        source:
          "<Trans>To continue, <strong>please <em>confirm</em></strong> your choice.</Trans>",
        build:
          '<Trans id="iU3T6K">' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<em>{@render __children()}</em>{/if}" +
          "{#if __index === 1}<strong>{@render __children()}</strong>{/if}" +
          "{/snippet}" +
          "</Trans>",
        dev:
          '<Trans id="iU3T6K" message={"To continue, <1>please <0>confirm</0></1> your choice."}>' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<em>{@render __children()}</em>{/if}" +
          "{#if __index === 1}<strong>{@render __children()}</strong>{/if}" +
          "{/snippet}" +
          "</Trans>",
      },
      {
        case: "with tags and variables",
        source:
          '<Trans>Hi {name}, please <a href="/profile"><strong>update</strong> your profile</a> or <em>contact {"support"}</em> for help.<br/>Thank you!</Trans>',
        build:
          '<Trans id="+xfjDc" values={{ 0: "support", name }}>' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<strong>{@render __children()}</strong>{/if}" +
          '{#if __index === 1}<a href="/profile">{@render __children()}</a>{/if}' +
          "{#if __index === 2}<em>{@render __children()}</em>{/if}" +
          "{#if __index === 3}<br/>{/if}" +
          "{/snippet}" +
          "</Trans>",
        dev:
          '<Trans id="+xfjDc" message={"Hi {name}, please <1><0>update</0> your profile</1> or <2>contact {0}</2> for help.<3/>Thank you!"} values={{ 0: "support", name }}>' +
          "{#snippet renderNode(__children, __index)}" +
          "{#if __index === 0}<strong>{@render __children()}</strong>{/if}" +
          '{#if __index === 1}<a href="/profile">{@render __children()}</a>{/if}' +
          "{#if __index === 2}<em>{@render __children()}</em>{/if}" +
          "{#if __index === 3}<br/>{/if}" +
          "{/snippet}" +
          "</Trans>",
      },
      {
        case: "with comments",
        source:
          '<Trans><!-- Comment -->Click <a href="/upgrade"><!-- Comment -->here<!-- Comment --></a> to upgrade</Trans>',
        build:
          '<Trans id="RUiQ7+">' +
          "{#snippet renderNode(__children, __index)}" +
          '{#if __index === 0}<a href="/upgrade">{@render __children()}</a>{/if}' +
          "{/snippet}" +
          "</Trans>",
        dev:
          '<Trans id="RUiQ7+" message={"Click <0>here</0> to upgrade"}>' +
          "{#snippet renderNode(__children, __index)}" +
          '{#if __index === 0}<a href="/upgrade">{@render __children()}</a>{/if}' +
          "{/snippet}" +
          "</Trans>",
      },
    ])("should transform code $case", ({ source, build, dev }) => {
      expect(svelteTransform(true, source).code).toEqual(build);
      expect(svelteTransform(false, source).code).toEqual(dev);
    });
  });
});
