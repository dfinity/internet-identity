Rules that apply to every language when translating `.po` files in this directory.

- Analyze existing translations in the file first to determine the established tone (e.g., formal/informal "you"), style, and terminology, and stay consistent with them.
- Preserve all variables (like `{variable}`) and tags (like `<0>...</0>`).
- Even for RTL languages, keep tags in the same order as the source text — do not reverse them.
- Apply the correct ICU plural categories for the target language (e.g. `one`/`other`, `one`/`few`/`many`/`other`, or just `other`). `other` is mandatory; additional categories are optional. If a target-language category is not explicitly present, ICU falls back to `other`.
- If the source plural has fewer categories than the target language, add target-language categories when needed for natural grammar. It is acceptable to keep fewer categories (such as only `one` + `other`) when that remains grammatically natural.
- Do not translate brand names.
