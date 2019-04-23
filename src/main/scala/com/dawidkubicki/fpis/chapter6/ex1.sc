import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG}

val rng = SimpleRNG(42)

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, rng2) = rng.nextInt
  val nn =
    if (n > 0) n
    else if (n == Int.MinValue) 0
    else -n
  (nn, rng2)
}

def rngLoop(rng: RNG, count: Int): Unit = {
  (1 to count).foldLeft(rng) { (r, _) =>
    val (nn, rr) = r.nextInt
    println(nn)
    rr
  }
}

def rngLoop2(rng: RNG, count: Int): Unit = {
  (1 to count).foldLeft(rng) { (r, _) =>
    val (nn, rr) = nonNegativeInt(r)
    println(nn)
    rr
  }
}

rngLoop(rng, 10)

rngLoop2(rng, 10)
