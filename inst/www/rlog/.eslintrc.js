module.exports = {
  "plugins": [
    "flowtype"
  ],
  "extends": [
    "plugin:flowtype/recommended",
    "eslint:recommended"
  ],
  "rules": {
    "consistent-return": 1,
    "dot-location": [1, "property"],
    "eqeqeq": 1,
    // "no-shadow": 1,
    "no-undef": 1,
    "no-unused-vars": [1, {"args": "none"}],
    "guard-for-in": 1,
    // "no-use-before-define": [1, {"functions": false}],
    "semi": [1, "always"]
  },
  "parserOptions": { "ecmaVersion": 6 }, // enable es6 language
  "env": { "es6": true }, // enable es6 objects, ex: Set
}
