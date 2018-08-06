let average a b => (a +. b) /. 2.0;

let add_int_float a b => float_of_int a +. b;

let rec range a b =>
  if (a > b) {
    []
  } else {
    [a, ...range (a + 1) b]
  };

let positive_sum a b => {
  let a = max a 0;
  let b = max b 0;
  a + b
};

let give_me_a_three _ => 3;
