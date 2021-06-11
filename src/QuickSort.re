open Swap;

let quicksort = t => {
  let split = (start, length, pivot_pos) => {
    let pivot = t[pivot_pos];
    swap(start, pivot_pos, t);
    let (low, high) = (ref(start + 1), ref(start + length - 1));
    while (low^ < high^) {
      while (low^ < high^ && t[low^] <= pivot) {
        incr(low);
      };
      while (low^ < high^ && t[high^] >= pivot) {
        decr(high);
      };
      if (low^ < high^) {
        swap(low^, high^, t);
      };
    };
    if (t[low^] > pivot) {
      decr(low);
    };
    swap(start, low^, t);
    low^;
  };

  let rec sort = (start, length) =>
    if (length > 1) {
      let pivot_pos = start + Random.int(length);
      let new_pos = split(start, length, pivot_pos);
      sort(start, new_pos - start);
      sort(new_pos + 1, start + length - new_pos - 1);
    };

  sort(0, Array.length(t));
};

let r_quicksort = l => {
  let split = (list, pivot) => {
    let rec _split = (inf, sup) =>
      fun
      | [] => (inf, sup)
      | [h, ...t] =>
        if (h < pivot) {
          _split([h, ...inf], sup, t);
        } else {
          _split(inf, [h, ...sup], t);
        };

    _split([], [], list);
  };

  let rec sort = result =>
    fun
    | [] => result
    | [a] => [a, ...result]
    | [pivot, ...t] => {
        let (inf, sup) = split(t, pivot);
        sort([pivot, ...sort(result, inf)], sup);
      };

  List.rev(sort([], l))|>ignore;
};
