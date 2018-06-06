module.exports = {
  "parser": "babel-eslint",
  "plugins": [
    "flowtype"
  ],
  "extends": [
    "plugin:flowtype/recommended",
    "eslint:recommended"
  ],
  "rules": {
    "flowtype/space-after-type-colon": 0, // fixed / clashes with prettier
    "comma-dangle": [2, "only-multiline"],
    "consistent-return": 1,
    "dot-location": [1, "property"],
    "eqeqeq": 2,
    // "no-shadow": 1,
    "no-undef": 2,
    "no-unused-vars": [1, {"args": "none"}],
    "guard-for-in": 2,
    // "no-use-before-define": [1, {"functions": false}],
    "semi": [2, "always"],
    "no-fallthrough": 0,
    "no-console": 2, // ["error", { allow: ["warn", "error"] }]
    "no-var": 2
  },
  "parserOptions": { "ecmaVersion": 6 }, // enable es6 language
  "env": {
    "browser": true,
    "es6": true // enable es6 objects, ex: Set
   },
}
