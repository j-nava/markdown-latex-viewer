'use strict';

const sanitizeHtml = require('sanitize-html');

exports.sanitize = function(html) {
  return function() {
    return sanitizeHtml(html);
  }
}
