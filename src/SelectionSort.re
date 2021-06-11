open Swap;

let selection_sort = t => {
  let length = Array.length(t);
  for (current in 0 to length - 1) {
    let min_id = ref(current);
    for (pos in current to length - 1) {
      min_id :=
        (
          if (t[pos] < t[min_id^]) {
            pos;
          } else {
            min_id^;
          }
        );
    };
    swap(current, min_id^, t);
  };
};

let  r_selection_sort = l => {
  let rec get_min =
    fun
    | [] => failwith("Empty")
    | [m] => m
    | [h, ...t] => min(h, get_min(t));

  let rec rem_min = m =>
    fun
    | [] => failwith("Empty")
    | [h, ...t] =>
      if (h == m) {
        t;
      } else {
        [h, ...rem_min(m, t)];
      };

  let rec sort =
    fun
    | [] => []
    | liste => {
        let min_val = get_min(liste);
        [min_val, ...sort(rem_min(min_val, liste))];
      };

  sort(l)|>ignore;
};

let  r_selection_sort2 = l => {
  let rec extract =
    fun
    | [] => failwith("Empty")
    | [a] => (a, [])
    | [head, ...tail] => {
        let (tail_min, newtail) = extract(tail);
        if (tail_min > head) {
          (head, [tail_min, ...newtail]);
        } else {
          (tail_min, [head, ...newtail]);
        };
      };

  let rec sort =
    fun
    | [] => []
    | liste => {
        let (min, tail) = extract(liste);
        [min, ...sort(tail)];
      };

  sort(l)|>ignore;
};
