let swap = (a, b, t) => {
  let t_b = t[b];
  t[b] = t[a];
  t[a] = t_b;
};
