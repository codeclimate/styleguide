module.exports = {
  "env": {
    "node": true,     // When in a backend context
    "browser": true,  // When in a web context
    "jquery": true,   // When in a web context
    "es6": true,      // When using ES6 features
  },
  "rules": {
    "brace-style": [2, "1tbs", { "allowSingleLine": true }],
    "camelcase": [2, { "properties": "always" }],
    "comma-style": [2, "first", { exceptions: {ArrayExpression: true, ObjectExpression: true} }],
    "complexity": [2, 6],
    "curly": 2,
    "eqeqeq": [2, "allow-null"],
    "no-shadow-restricted-names": 2,
    "no-ternary": 2,
    "no-undef": 2,
    "no-unused-vars": [2, { "argsIgnorePattern": "^_" }],
    "no-use-before-define": 2,
    "quotes": [2, "double", "avoid-escape"],
    "radix": 2,
    "semi": 2,
    "space-infix-ops": 2,
    "strict": 0,
  },
  /**
   *  globals should be defined per file when possible. Use the directive here
   *  when there are project-level globals (such as jquery)
   */
  "globals": {},
};
