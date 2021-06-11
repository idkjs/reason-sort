let merge_sort = t => {
  let merge = (t1, length1, t2, length2) => {
    let out = Array.make(length1 + length2, 0);
    let (pos1, pos2) = (ref(0), ref(0));
    while (pos1^ < length1 || pos2^ < length2) {
      let pos = pos1^ + pos2^;
      if (pos1^ == length1) {
        out[pos] = t2[pos2^];
        incr(pos2);
      } else if (pos2^ == length2) {
        out[pos] = t1[pos1^];
        incr(pos1);
      } else if (t1[pos1^] < t2[pos2^]) {
        out[pos] = t1[pos1^];
        incr(pos1);
      };
    };
    out;
  };

  let rec sort = (start, length) =>
    if (length == 0) {
      [||];
    } else if (length == 1) {
      [|t[start]|];
    } else {
      let start1 = start
      and length1 = length / 2;
      let start2 = start1 + length1
      and length2 = length - length1;
      merge(sort(start1, length1), length1, sort(start2, length2), length2);
    };

  let size = Array.length(t);
  Array.blit(sort(0, size), 0, t, 0, size);
};

/* For the recursive version, we define a split function as follows: */

let split = l => {
  let rec _split = (source, left, right) =>
    switch (source, right) {
    | ([], _)
    | ([_], _) => (left, right)
    | ([_, _, ...tail], [r, ...right_tail]) =>
      _split(tail, [r, ...left], right_tail)
    | _ => assert(false)
    };

  _split(l, [], l);
};

/* The split function can also be implemented as follows (a little slower
   though): let split let rec match l= _split source left right = source , right
   with let split l = */
let split2 = l => {
  let rec _split = (source, left, right) =>
    switch (source) {
    | [] => (left, right)
    | [head, ...tail] => _split(tail, right, [head, ...left])
    };

  _split(l, []);
};

let r_merge_sort = l => {
  let rec merge = (l1, l2) =>
    switch (l1, l2) {
    | ([], l)
    | (l, []) => l
    | ([h1, ...t1], [h2, ...t2]) =>
      if (h1 < h2) {
        [h1, ...merge(t1, [h2, ...t2])];
      } else {
        [h2, ...merge([h1, ...t1], t2)];
      }
    };

  let rec sort =
    fun
    | ([] | [_]) as sorted => sorted
    | list => {
        let (left, right) = split(list);
        merge(sort(left), sort(right));
      };

  sort(l)|>ignore;
};

let tr_merge_sort = l => {
  let merge = (l1, l2) => {
    let rec _merge = (l1, l2, result) =>
      switch (l1, l2) {
      | ([], []) => result
      | ([], [h, ...t])
      | ([h, ...t], []) => _merge([], t, [h, ...result])
      | ([h1, ...t1], [h2, ...t2]) =>
        if (h1 < h2) {
          _merge(t1, l2, [h1, ...result]);
        } else {
          _merge(l1, t2, [h2, ...result]);
        }
      };

    List.rev(_merge(l1, l2, []));
  };

  let rec sort = (l, merge_fn) =>
    switch (l) {
    | []
    | [_] => merge_fn(l)
    | list =>
      let (left, right) = split(list);
      sort(left, leftR =>
        sort(right, rightR => merge_fn(merge(leftR, rightR)))
      );
    };

  sort(l, x => x)|>ignore;
};
