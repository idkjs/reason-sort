// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Swap$ReasonSort = require("./Swap.bs.js");

function heap_sort(t) {
  var max_size = t.length;
  var heap = {
    data: Caml_array.caml_make_vect(max_size + 1 | 0, 0),
    size: 0
  };
  var push = function (x) {
    if (heap.size === max_size) {
      Pervasives.failwith("Heap overflow");
    }
    Caml_array.set(heap.data, heap.size + 1 | 0, x);
    var pos = heap.size + 1 | 0;
    while(pos !== 1) {
      var newpos = pos / 2 | 0;
      if (Caml_array.get(heap.data, newpos) > Caml_array.get(heap.data, pos)) {
        Swap$ReasonSort.swap(pos, newpos, heap.data);
      }
      pos = newpos;
    };
    heap.size = heap.size + 1 | 0;
    
  };
  var pop = function (param) {
    if (heap.size === 0) {
      Pervasives.failwith("Empty heap");
    }
    var top = Caml_array.get(heap.data, 1);
    Caml_array.set(heap.data, 1, Caml_array.get(heap.data, heap.size));
    var changed = true;
    var pos = 1;
    while(changed && (pos << 1) <= heap.size) {
      var minpos = pos;
      var left = (pos << 1);
      var right = (pos << 1) + 1 | 0;
      if (Caml_array.get(heap.data, minpos) > Caml_array.get(heap.data, left)) {
        minpos = left;
      }
      if (right <= heap.size && Caml_array.get(heap.data, minpos) > Caml_array.get(heap.data, right)) {
        minpos = right;
      }
      changed = pos !== minpos;
      Swap$ReasonSort.swap(pos, minpos, heap.data);
      pos = minpos;
    };
    heap.size = heap.size - 1 | 0;
    return top;
  };
  for(var pos = 0; pos < max_size; ++pos){
    push(Caml_array.get(t, pos));
  }
  for(var pos$1 = 0; pos$1 < max_size; ++pos$1){
    Caml_array.set(t, pos$1, pop(undefined));
  }
  
}

exports.heap_sort = heap_sort;
/* No side effect */
