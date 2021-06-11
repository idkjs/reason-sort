// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";
import * as Swap$ReasonSort from "./Swap.bs.js";

function selection_sort(t) {
  var length = t.length;
  for(var current = 0; current < length; ++current){
    var min_id = current;
    for(var pos = current; pos < length; ++pos){
      min_id = Caml_obj.caml_lessthan(Caml_array.get(t, pos), Caml_array.get(t, min_id)) ? pos : min_id;
    }
    Swap$ReasonSort.swap(current, min_id, t);
  }
  
}

function r_selection_sort(l) {
  var get_min = function (param) {
    if (!param) {
      return Pervasives.failwith("Empty");
    }
    var t = param.tl;
    var m = param.hd;
    if (t) {
      return Caml_obj.caml_min(m, get_min(t));
    } else {
      return m;
    }
  };
  var rem_min = function (m, param) {
    if (!param) {
      return Pervasives.failwith("Empty");
    }
    var t = param.tl;
    var h = param.hd;
    if (Caml_obj.caml_equal(h, m)) {
      return t;
    } else {
      return {
              hd: h,
              tl: rem_min(m, t)
            };
    }
  };
  var sort = function (liste) {
    if (!liste) {
      return /* [] */0;
    }
    var min_val = get_min(liste);
    return {
            hd: min_val,
            tl: sort(rem_min(min_val, liste))
          };
  };
  sort(l);
  
}

function r_selection_sort2(l) {
  var extract = function (param) {
    if (!param) {
      return Pervasives.failwith("Empty");
    }
    var tail = param.tl;
    var a = param.hd;
    if (!tail) {
      return [
              a,
              /* [] */0
            ];
    }
    var match = extract(tail);
    var newtail = match[1];
    var tail_min = match[0];
    if (Caml_obj.caml_greaterthan(tail_min, a)) {
      return [
              a,
              {
                hd: tail_min,
                tl: newtail
              }
            ];
    } else {
      return [
              tail_min,
              {
                hd: a,
                tl: newtail
              }
            ];
    }
  };
  var sort = function (liste) {
    if (!liste) {
      return /* [] */0;
    }
    var match = extract(liste);
    return {
            hd: match[0],
            tl: sort(match[1])
          };
  };
  sort(l);
  
}

export {
  selection_sort ,
  r_selection_sort ,
  r_selection_sort2 ,
  
}
/* No side effect */