print("N: ");
let n = int(getline());

fn fact(x) {
  if x == 0 {
    0
  } else if x == 1 {
    1
  } else {
    fact(x - 1) * x
  }
}

fn fact2(x) {
  if x < 1 { return 0; }

  let res = 1;
  while x > 0 { res := res * x--; }

  return res;
}

println("N! rec:", fact(n));
println("N! iter:", fact2(n));
