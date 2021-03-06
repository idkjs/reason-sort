// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Swap$ReasonSort from "./Swap.bs.js";

function bubble_sort(t) {
  var length = t.length;
  for(var max_pos = length - 1 | 0; max_pos >= 0; --max_pos){
    for(var pos = 0; pos < max_pos; ++pos){
      if (Caml_obj.caml_greaterthan(Caml_array.get(t, pos), Caml_array.get(t, pos + 1 | 0))) {
        Swap$ReasonSort.swap(pos, pos + 1 | 0, t);
      }
      
    }
  }
  
}

function r_bubble_sort(l) {
  var try_swap = function (param) {
    if (!param) {
      return [
              false,
              /* [] */0
            ];
    }
    var match = param.tl;
    var a = param.hd;
    if (!match) {
      return [
              false,
              {
                hd: a,
                tl: /* [] */0
              }
            ];
    }
    var tail = match.tl;
    var b = match.hd;
    if (Caml_obj.caml_greaterthan(a, b)) {
      return [
              true,
              {
                hd: b,
                tl: try_swap({
                        hd: a,
                        tl: tail
                      })[1]
              }
            ];
    }
    var match$1 = try_swap({
          hd: b,
          tl: tail
        });
    return [
            match$1[0],
            {
              hd: a,
              tl: match$1[1]
            }
          ];
  };
  var sort = function (_l) {
    while(true) {
      var l = _l;
      var match = try_swap(l);
      var newlist = match[1];
      if (!match[0]) {
        return newlist;
      }
      _l = newlist;
      continue ;
    };
  };
  sort(l);
  
}

export {
  bubble_sort ,
  r_bubble_sort ,
  
}
/* No side effect */
