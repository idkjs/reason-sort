open Swap;

let insert_sort = t => {
  let length = Array.length(t);
  for (start in 1 to length - 1) {
    let pos = ref(start);
    while (pos^ > 0 && t[pos^ - 1] > t[pos^]) {
      swap(pos^ - 1, pos^, t);
      decr(pos);
    };
  };
};

let r_insert_sort = l => {
  let rec insert = value =>
    fun
    | [] => [value]
    | [h, ...t] =>
      if (h > value) {
        [value, h, ...t];
      } else {
        [h, ...insert(value, t)];
      };

  let rec sort =
    fun
    | [] => []
    | [h, ...t] => insert(h, sort(t));
  sort(l)|>ignore;
};
