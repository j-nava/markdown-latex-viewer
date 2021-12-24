'use strict';

const katex = require('katex');

exports._parse = function(text) {
  return function (left) {
    return function (right) {
      return function() {
        try {
          var parsed = katex.renderToString(text, { throwOnError: true, output: "html" });
          return right(parsed);
        }
        catch (e) {
          return left(e);
        }
      }
    }
  }
}
