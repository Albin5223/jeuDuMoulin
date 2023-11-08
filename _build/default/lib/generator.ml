let randomAlea n = 
  Random.self_init ();
  Random.int n

let randomSeed = Random.init 7

let randomAleaWithSeed n =
  randomSeed;Random.int n
