// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Swap$ReasonSort = require("./Swap.bs.js");

function insert_sort(t) {
  var length = t.length;
  for(var start = 1; start < length; ++start){
    var pos = start;
    while(pos > 0 && Caml_obj.caml_greaterthan(Caml_array.get(t, pos - 1 | 0), Caml_array.get(t, pos))) {
      Swap$ReasonSort.swap(pos - 1 | 0, pos, t);
      pos = pos - 1 | 0;
    };
  }
  
}

function r_insert_sort(l) {
  var insert = function (value, param) {
    if (!param) {
      return {
              hd: value,
              tl: /* [] */0
            };
    }
    var t = param.tl;
    var h = param.hd;
    if (Caml_obj.caml_greaterthan(h, value)) {
      return {
              hd: value,
              tl: {
                hd: h,
                tl: t
              }
            };
    } else {
      return {
              hd: h,
              tl: insert(value, t)
            };
    }
  };
  var sort = function (param) {
    if (param) {
      return insert(param.hd, sort(param.tl));
    } else {
      return /* [] */0;
    }
  };
  sort(l);
  
}

exports.insert_sort = insert_sort;
exports.r_insert_sort = r_insert_sort;
/* No side effect */
