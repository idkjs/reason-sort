open Swap;

let bubble_sort = t => {
  let length = Array.length(t);
  for (max_pos in length - 1 downto 0) {
    for (pos in 0 to max_pos - 1) {
      if (t[pos] > t[pos + 1]) {
        swap(pos, pos + 1, t);
      };
    };
  };
};
let r_bubble_sort = l => {
  let rec try_swap =
    fun
    | [] => (false, [])
    | [a] => (false, [a])
    | [a, b, ...tail] =>
      if (a > b) {
        (true, [b, ...snd(try_swap([a, ...tail]))]);
      } else {
        let (swapped, newlist) = try_swap([b, ...tail]);
        (swapped, [a, ...newlist]);
      };
  let rec sort = l => {
    let (swapped, newlist) = try_swap(l);
    if (swapped) {
      sort(newlist);
    } else {
      newlist;
    };
  };
  sort(l)|>ignore;
};
// let r_bubble_sort = l => {
//   let rec try_swap = next =>
//     switch (next) {
//     | [] => (false, [])
//     | [a] => (false, [a])
//     | [a, b, ...tail] =>
//       if (a > b) {
//         (true, [b, ...snd(try_swap([a, ...tail]))]);
//       } else {
//         let (swapped, newlist) = try_swap([b, ...tail]);
//         (swapped, [a, ...newlist]);
//       }
//     };

//   let rec sort = l => {
//     let (swapped, newlist) = try_swap(l);
//     if (swapped) {
//       sort(newlist);
//     } else {
//       newlist;
//     };
//   };

//   sort(l);
// };
