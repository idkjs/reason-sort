open BubbleSort;
open HeapSort;
open InsertSort;
open MergeSort;
open QuickSort;
open SelectionSort;

// https://stackoverflow.com/questions/30867898/what-is-the-workaround-for-ocaml-exception-invalid-argumentrandom-int
let my_max_int = 1 lsl 25 - 1;
let random_vect = length => {
  let t = Array.make(length, 0);
  for (pos in 0 to length - 1) {
    // t[pos] = Random.bits();
    t[pos] =
      Random.int(my_max_int);
  };
  t;
};

let rec random_list =
  fun
  | 0 => []
  | n => [Random.int(my_max_int), ...random_list(n - 1)];

let time = (randomize, sort, length) => {
  let data = randomize(length);
  let start = Sys.time();
  sort(data);
  Sys.time() -. start;
};

let time_fn = (name, random, fn, counts) => {
  let rec _time_fn =
    fun
    | [] => ()
    | [count, ...tail] => {
        Printf.printf(
          "%s : %d elements -> %f\n",
          name,
          count,
          time(random, fn, count),
        );
        _time_fn(tail);
      };

  _time_fn(counts);
  print_newline();
};


type suite = {
  name: string,
  fn: unit => unit,
};

let bench = ({name, fn}) => {
  let t0 = Js.Date.now();
  let result = fn();
  let t1 = Js.Date.now();
  Printf.printf("%20s : %.3f s\n%!", name, t1 -. t0);
  result;
};
let suite = [
  {
    name: "bubble_sort",
    fn: () =>
      time_fn(
        "bubble_sort",
        random_vect,
        bubble_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "insert_sort",
    fn: () =>
      time_fn(
        "insert_sort",
        random_vect,
        insert_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "selection_sort",
    fn: () =>
      time_fn(
        "selection_sort",
        random_vect,
        selection_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "quicksort",
    fn: () =>
      time_fn(
        "quicksort",
        random_vect,
        quicksort,
        [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "merge_sort",
    fn: () =>
      time_fn(
        "merge_sort",
        random_vect,
        merge_sort,
        [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "heap_sort",
    fn: () =>
      time_fn(
        "heap_sort",
        random_vect,
        heap_sort,
        [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "Array.sort",
    fn: () =>
      time_fn(
        "Array.sort",
        random_vect,
        Array.sort(compare),
        [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "r_bubble_sort",
    fn: () =>
      time_fn(
        "r_bubble_sort",
        random_list,
        r_bubble_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "r_insert_sort",
    fn: () =>
      time_fn(
        "r_insert_sort",
        random_list,
        r_insert_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "r_selection_sort",
    fn: () =>
      time_fn(
        "r_selection_sort",
        random_list,
        r_selection_sort,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "r_selection_sort2",
    fn: () =>
      time_fn(
        "r_selection_sort2",
        random_list,
        r_selection_sort2,
        [10, 100, 1000, 10000],
      ),
  },
  {
    name: "r_quicksort",
    fn: () =>
      time_fn(
        "r_quicksort",
        random_list,
        r_quicksort,
        [10, 100, 1000, 10000, 100000],
        // stackoverflow at 1000000
        // [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "r_merge_sort",
    fn: () =>
      time_fn(
        "r_merge_sort",
        random_list,
        r_merge_sort,
        [10, 100, 1000, 10000, 100000],
        // stackoverflow at 1000000
        // [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  {
    name: "tr_merge_sort",
    fn: () =>
      time_fn(
        "tr_merge_sort",
        random_list,
        tr_merge_sort,
        [10, 100, 1000, 10000, 100000],
        // stackoverflow at 1000000
        // [10, 100, 1000, 10000, 100000, 1000000],
      ),
  },
  // time_fn(
  //   "List.sort",
  //   random_list,
  //   (List.sort(compare)),
  //   [10, 100, 1000, 10000, 100000, 1000000],
  // ),
];
List.iter(t => bench(t), suite);


// let suite = [
//   time_fn("bubble_sort", random_vect, bubble_sort, [10, 100, 1000, 10000]),
//   time_fn("insert_sort", random_vect, insert_sort, [10, 100, 1000, 10000]),
//   time_fn(
//     "selection_sort",
//     random_vect,
//     selection_sort,
//     [10, 100, 1000, 10000],
//   ),
//   time_fn(
//     "quicksort",
//     random_vect,
//     quicksort,
//     [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "merge_sort",
//     random_vect,
//     merge_sort,
//     [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "heap_sort",
//     random_vect,
//     heap_sort,
//     [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "Array.sort",
//     random_vect,
//     Array.sort(compare),
//     [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "r_bubble_sort",
//     random_list,
//     r_bubble_sort,
//     [10, 100, 1000, 10000],
//   ),
//   time_fn(
//     "r_insert_sort",
//     random_list,
//     r_insert_sort,
//     [10, 100, 1000, 10000],
//   ),
//   time_fn(
//     "r_selection_sort",
//     random_list,
//     r_selection_sort,
//     [10, 100, 1000, 10000],
//   ),
//   time_fn(
//     "r_selection_sort2",
//     random_list,
//     r_selection_sort2,
//     [10, 100, 1000, 10000],
//   ),
//   time_fn(
//     "r_quicksort",
//     random_list,
//     r_quicksort,
//     [10, 100, 1000, 10000, 100000],
//     // stackoverflow at 1000000
//     // [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "r_merge_sort",
//     random_list,
//     r_merge_sort,
//     [10, 100, 1000, 10000, 100000],
//     // stackoverflow at 1000000
//     // [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   time_fn(
//     "tr_merge_sort",
//     random_list,
//     tr_merge_sort,
//     [10, 100, 1000, 10000, 100000],
//     // stackoverflow at 1000000
//     // [10, 100, 1000, 10000, 100000, 1000000],
//   ),
//   // time_fn(
//   //   "List.sort",
//   //   random_list,
//   //   (List.sort(compare)),
//   //   [10, 100, 1000, 10000, 100000, 1000000],
//   // ),
// ];
