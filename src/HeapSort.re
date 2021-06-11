open Swap;

type heap_struct = {
  data: array(int),
  mutable size: int,
};

let heap_sort = t => {
  let max_size = Array.length(t);
  let heap = {data: Array.make(max_size + 1, 0), size: 0};
  let push = x => {
    if (heap.size == max_size) {
      failwith("Heap overflow");
    };
    heap.data[heap.size + 1] = x;
    let pos = ref(heap.size + 1);
    while (pos^ != 1) {
      let newpos = pos^ / 2;
      if (heap.data[newpos] > heap.data[pos^]) {
        swap(pos^, newpos, heap.data);
      };
      pos := newpos;
    };
    heap.size = heap.size + 1;
  };

  let pop = () => {
    if (heap.size == 0) {
      failwith("Empty heap");
    };
    let top = heap.data[1];
    heap.data[1] = heap.data[heap.size];
    let changed = ref(true);
    let pos = ref(1);
    while (changed^ && 2 * pos^ <= heap.size) {
      let minpos = ref(pos^);
      let left = 2 * pos^
      and right = 2 * pos^ + 1;
      if (heap.data[minpos^] > heap.data[left]) {
        minpos := left;
      };
      if (right <= heap.size) {
        if (heap.data[minpos^] > heap.data[right]) {
          minpos := right;
        };
      };
      changed := !(pos^ == minpos^);
      swap(pos^, minpos^, heap.data);
      pos := minpos^;
    };
    heap.size = heap.size - 1;
    top;
  };

  for (pos in 0 to max_size - 1) {
    push(t[pos]);
  };
  for (pos in 0 to max_size - 1) {
    t[pos] = pop();
  };
};
