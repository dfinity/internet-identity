# Localizing

This document explains how to localize Internet Identity into other languages.

## Pre-requisites

Make sure Node.js has been installed and this project dependencies have been installed:

```bash
npm ci
```

## Adding languages

To add another language, add another IETF BCP 47 language tag to the `availableLocales` variable in
`src/frontend/src/lib/constants/locale.constants.ts`.

After this, you can follow below [Updating translations](#Updating-translations) instructions.

## Updating translations

### Extracting

First update the locale catalogs with the latest changes from the frontend implementation:

```bash
npm run extract
```

### Catalogs

The localization files can be found and directly edited at `/src/frontend/src/lib/locales/{locale}.po`.

See [HACKING](./HACKING.md) for instructions to run II locally to see the locale changes.

## Development

The Svelte implementation is built upon [Lingui](https://lingui.dev/), see their docs for more detailed documentation.

### Simple text

Wrap your string in `$t`:

```sveltehtml

<script lang="ts">
  import { t } from "$lib/stores/locale.store";
</script>

<p>{$t`Hello world`}</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Hello world"
msgstr "Hello world"
```

### Accessibility labels

Attributes like aria-labels can also be translated:

```sveltehtml

<script lang="ts">
  import { HelpCircleIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
</script>

<Button iconOnly aria-label={$t`Learn more`}>
  <HelpCircleIcon />
</Button>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Hello world"
msgstr "Hello world"
```

### Text with variables

Reference your variables directly in the string:

```sveltehtml

<script lang="ts">
  import { t } from "$lib/stores/locale.store";

  const name = $state("John");
</script>

<p>{$t`Hello ${name}}`}</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Hello {name}"
msgstr "Hello {name}"
```

> ⚠️ **NOTE**  
> The id changes when a variable renamed, which means it'll need translated into other languages again.

Indexes will be used as fallback if a direct value is used instead:

```sveltehtml

<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { namesApi } from "$lib/apis/name";
</script>

<p>{$t`Hello ${namesApi.getName()}}`}</p>
```

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Hello {0}"
msgstr "Hello {0}"
```

> ⚠️ **NOTE**  
> Referencing variables is recommended over using direct values. Named variables, particularly when there's multiple,
> add important contextual information for translators.

### Additional context

Additional context can be added for translators where needed to make sure things are translated into other languages
with their correct meaning and intent:

```sveltehtml

<script lang="ts">
  import { t } from "$lib/stores/locale.store";
</script>

<h1
  class="text-text-disabled text-center text-4xl md:text-5xl lg:text-7xl"
>
  {$t({
    message: "Experience",
    context:
      "Used as an action word inviting the reader to try or feel something, e.g. Experience Real Privacy",
  })}
</h1>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgctxt "Used as an action word inviting the reader to try or feel something, e.g. Experience Real Privacy"
msgid "Experience"
msgstr "Experience"
```

### Explicit ID

An explicit ID can be defined for various use-cases e.g. the localized variants of a product:

```sveltehtml

<script lang="ts">
  import { t } from "$lib/stores/locale.store";
</script>

<p>
  {$t({
    id: "COCA_COLA",
    message: "Coca-Cola",
  })}
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "COCA_COLA"
msgstr "Coca-Cola"
```

Translation in `zh-CN.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "COCA_COLA"
msgstr "可口可乐"
```

> Means "tasty and happy" — brilliant localization that sounds like "Coca-Cola".

> ⚠️ **NOTE**  
> It's recommended to not use explicit IDs where possible, see more on this topic
> here: https://lingui.dev/guides/explicit-vs-generated-ids

### Pluralization

Since we use English in the source code, we'll only need to use one and other:

```sveltehtml

<script lang="ts">
  import { plural } from "$lib/stores/locale.store";

  let numBooks = $state(3);
</script>

<p>
  {$plural(numBooks, {
    one: "One book",
    other: "# books"
  })}
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "{numBooks, plural, one {One book} other {# books}}"
msgstr "{numBooks, plural, one {One book} other {# books}}"
```

Messages for exact values like 0 can be additionally defined:

```sveltehtml

<script lang="ts">
  import { plural } from "$lib/stores/locale.store";

  let numBooks = $state(3);
</script>

<p>
  {$plural(numBooks, {
    "=0": "No books",
    one: "One book",
    other: "# books"
  })}
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "{num, plural, =0 {No books} one {One book} other {# books}}"
msgstr "{num, plural, =0 {No books} one {One book} other {# books}}"
```

Variables are also supported:

```sveltehtml

<script lang="ts">
  import { plural } from "$lib/stores/locale.store";

  let numBooks = $state(3);
  let genre = $state('fantasy');
</script>

<p>
  {$plural(numBooks, {
    one: `One ${genre} book`,
    other: `# ${genre} books`
  })}
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "{num, plural, one {One {genre} book} other {# {genre} books}}"
msgstr "{num, plural, one {One {genre} book} other {# {genre} books}}"
```

### Nested components

The `<Trans>` component is available for cases where you have nested components:

```sveltehtml

<script lang="ts">
  import { Trans } from "$lib/components/locale";
</script>

<p>
  <Trans>Click <a href="/upgrade">here</a> to upgrade</Trans>
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Click <0>here</0> to upgrade"
msgstr "Click <0>here</0> to upgrade"
```

Variable references and values are also supported:

```sveltehtml

<script lang="ts">
  import { Trans } from "$lib/components/locale";

  const name = $state("John");
</script>

<p>
  <Trans>
    Hi <b>{name}</b>!
    <br>
    Click <a href="/upgrade">here</a> to upgrade
  </Trans>
</p>
```

Extracted text in `en.po`:

```
#: src/frontend/src/routes/+page.svelte:10
msgid "Hi <0>{name}</0>!<1/>Click <2>here</2> to upgrade"
msgstr "Hi <0>{name}</0>!<1/>Click <2>here</2> to upgrade"
```