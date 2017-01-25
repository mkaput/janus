let i = 0;
while i <= 100 {
  if i mod 3 == 0 and i mod 5 == 0 {
    println("Janusze Haskella");
  } else if i mod 3 == 0 {
    println("Janusz");
  } else if i mod 5 == 0 {
    println("Haskell");
  } else {
    println(i);
  }

  i++;
}
