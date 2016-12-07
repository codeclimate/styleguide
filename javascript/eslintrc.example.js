module.exports = {
  "env": {
    "node": true,     // When in a backend context
    "browser": true,  // When in a web context
    "jquery": true,   // When in a web context
    "es6": true,      // When using ES6 features
  },
  // It's recommended these files be pulled in via `codeclimate prepare`
  extends: [
    ".eslintrc.base.js",
    ".eslintrc.es6.js" // only for ES6 projects
  ],
  /**
   *  globals should be defined per file when possible. Use the directive here
   *  when there are project-level globals (such as jquery)
   */
  "globals": {},
};
