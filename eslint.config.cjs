const js = require("@eslint/js");
const globals = require("globals");

module.exports = [
  {
    ...js.configs.recommended,
    rules: {
      ...js.configs.recommended.rules,
      "no-empty": ["error", { allowEmptyCatch: true }],
      "no-unused-vars": ["error", { caughtErrors: "none" }],
    },
  },
  {
    files: ["resources/status/**/*.js"],
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "script",
      globals: {
        ...globals.browser,
        ...globals.jquery,
        bootstrap: "readonly",
        DataTable: "readonly",
        DOMPurify: "readonly",
      },
    },
  },
  {
    files: [
      "tools/dompurify/**/*.js",
      "tools/readability/**/*.js",
      "tools/value-inspector/**/*.js",
    ],
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "commonjs",
      globals: globals.node,
    },
  },
  {
    files: [
      "demo/**/*.mjs",
      "tools/frontend/**/*.mjs",
      "tools/normalize-node-junit.mjs",
      "tools/save-to-llar-extension/test/**/*.mjs",
    ],
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "module",
      globals: {
        ...globals.nodeBuiltin,
        ...globals.browser,
        ...globals.jquery,
        bookmark_capture_error_message: "readonly",
        DataTable: "readonly",
        datatable_for: "readonly",
        readingNavigation: "readonly",
        readingScrollBehavior: "readonly",
        reload_dashboard_tab: "readonly",
        restoreReaderListPosition: "readonly",
        saveReaderListPosition: "readonly",
      },
    },
  },
  {
    files: ["tools/save-to-llar-extension/*.mjs"],
    languageOptions: {
      ecmaVersion: "latest",
      sourceType: "module",
      globals: globals.browser,
    },
  },
  {
    files: ["tools/normalize-node-junit.mjs"],
    rules: {
      "no-control-regex": "off",
    },
  },
];
