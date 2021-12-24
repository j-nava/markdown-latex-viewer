'use strict';

const { marked } = require('marked');

exports.parse = function(text) {
  return function() {
    return marked.parse(text);
  }
}