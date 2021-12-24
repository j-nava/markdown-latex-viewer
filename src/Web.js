'use strict';

exports.loadHtml = function(html) {
  return function() {
    document.getElementById("divText").innerHTML = html;
    return {};
  }
}
