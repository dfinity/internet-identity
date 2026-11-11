import prettier from "eslint-config-prettier";
import js from "@eslint/js";
import { includeIgnoreFile } from "@eslint/compat";
import svelte from "eslint-plugin-svelte";
import globals from "globals";
import { fileURLToPath } from "node:url";
import ts from "typescript-eslint";

const gitignorePath = fileURLToPath(new URL("./.gitignore", import.meta.url));

export default ts.config(
  includeIgnoreFile(gitignorePath),
  js.configs.recommended,
  ...ts.configs.recommended,
  ...svelte.configs["flat/recommended"],
  prettier,
  ...svelte.configs["flat/prettier"],
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
      },
    },
  },
  {
    files: ["**/*.svelte", "**/*.svelte.ts", "**/*.svelte.js"],

    languageOptions: {
      parserOptions: {
        parser: ts.parser,
        extraFileExtensions: [".svelte"],
      },
    },
  },
  {
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.eslint.json"],
        extraFileExtensions: [".svelte"],
      },
    },
  },
  {
    // The service worker and what it reaches. A worker has no document, so an import
    // that lands on page-only code throws at worker startup, where the only symptom
    // is a push that shows nothing. `globals` is bootstrapped from a page and
    // `iiConnection` imports it, so neither may be reached from here.
    files: [
      "src/frontend/src/service-worker.ts",
      "src/frontend/src/lib/utils/notifications/worker/**/*.ts",
    ],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          paths: [
            {
              name: "$lib/globals",
              message:
                "Page-only: bootstrapped from the page. A worker reads what it needs from workerConfig.",
            },
            {
              name: "$lib/utils/iiConnection",
              message:
                "Page-only: it imports $lib/globals. Take urlUtils or agentHost-free helpers instead.",
            },
          ],
        },
      ],
    },
  },
  {
    rules: {
      "@typescript-eslint/strict-boolean-expressions": [
        2,
        { allowString: false, allowNumber: false },
      ],
      "@typescript-eslint/no-unused-vars": [
        "warn",
        {
          argsIgnorePattern: "^_",
          varsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
        },
      ],
      "require-await": ["error"],
      "@typescript-eslint/no-floating-promises": ["error"],
      // @see https://github.com/sveltejs/eslint-plugin-svelte/issues/1353
      "svelte/no-navigation-without-resolve": "off",
    },
  },
  {
    ignores: ["src/frontend/src/lib/generated/*"],
  },
);
