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
    files: ["**/*.svelte"],

    languageOptions: {
      parserOptions: {
        parser: ts.parser,
      },
    },
  },
  {
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.json"],
      },
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
    },
  },
  {
    ignores: ["src/frontend/src/lib/generated/*", "src/showcase/.astro/*"],
  },
);
