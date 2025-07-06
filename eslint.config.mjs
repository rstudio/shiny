import typescriptEslint from "@typescript-eslint/eslint-plugin";
import prettier from "eslint-plugin-prettier";
import jestDom from "eslint-plugin-jest-dom";
import unicorn from "eslint-plugin-unicorn";
import globals from "globals";
import tsParser from "@typescript-eslint/parser";
import path from "node:path";
import { fileURLToPath } from "node:url";
import js from "@eslint/js";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
    baseDirectory: __dirname,
    recommendedConfig: js.configs.recommended,
    allConfig: js.configs.all
});

export default [{
    ignores: ["**/__tests__", "**/*.d.ts"],
}, ...compat.extends(
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:jest/recommended",
    "plugin:prettier/recommended",
    "plugin:jest-dom/recommended",
), {
    plugins: {
        "@typescript-eslint": typescriptEslint,
        prettier,
        "jest-dom": jestDom,
        unicorn,
    },

    languageOptions: {
        globals: {
            ...globals.browser,
            Atomics: "readonly",
            SharedArrayBuffer: "readonly",
        },

        parser: tsParser,
        ecmaVersion: 2021,
        sourceType: "module",

        parserOptions: {
            project: ["./tsconfig.json"],
        },
    },

    rules: {
        "@typescript-eslint/explicit-function-return-type": "off",
        "@typescript-eslint/no-explicit-any": "off",
        "@typescript-eslint/explicit-module-boundary-types": "error",
        "default-case": ["error"],
        "linebreak-style": ["error", "unix"],
        quotes: ["error", "double", "avoid-escape"],
        semi: ["error", "always"],
        "dot-location": ["error", "property"],
        camelcase: ["off"],

        "unicorn/filename-case": ["error", {
            case: "camelCase",
        }],

        "@typescript-eslint/array-type": ["error", {
            default: "array-simple",
            readonly: "array-simple",
        }],

        "@typescript-eslint/consistent-indexed-object-style": ["error", "index-signature"],
        "@typescript-eslint/consistent-type-imports": "error",
        "@typescript-eslint/no-floating-promises": "error",

        "@typescript-eslint/naming-convention": ["error", {
            selector: "default",
            format: ["camelCase"],
        }, {
            selector: "method",
            modifiers: ["private"],
            format: ["camelCase"],
            leadingUnderscore: "require",
        }, {
            selector: "method",
            modifiers: ["protected"],
            format: ["camelCase"],
            leadingUnderscore: "require",
        }, {
            selector: "variable",
            format: ["camelCase"],
            trailingUnderscore: "forbid",
            leadingUnderscore: "forbid",
        }, {
            selector: "parameter",
            format: ["camelCase"],
            trailingUnderscore: "allow",
            leadingUnderscore: "forbid",
        }, {
            selector: ["enum", "enumMember"],
            format: ["PascalCase"],
        }, {
            selector: "typeLike",
            format: ["PascalCase"],

            custom: {
                regex: "(t|T)ype$",
                match: false,
            },
        }],
    },
}];
