import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG}

val simpleRng: RNG = SimpleRNG(427)

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  (0 until count).foldLeft((List.empty[Int], rng)) { case ((ints, rng2), _) =>
    val (n, rng3) = rng2.nextInt
    (ints :+ n, rng3)
  }


ints(3)(simpleRng)._1
ints(5)(simpleRng)._1
ints(10)(simpleRng)._1

ints(10000)(simpleRng)._1
