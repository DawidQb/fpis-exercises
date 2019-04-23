import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG, Util}

val simpleRng: RNG = SimpleRNG(427)

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (int, rng2) = rng.nextInt
  val (dbl, rng3) = Util.double(rng2)
  ((int, dbl), rng3)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val (dbl, rng2) = Util.double(rng)
  val (int, rng3) = rng2.nextInt
  ((dbl, int), rng3)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (dbl, rng2) = Util.double(rng)
  val (dbl2, rng3) = Util.double(rng2)
  val (dbl3, rng4) = Util.double(rng3)
  ((dbl, dbl2, dbl3), rng4)
}

double3(simpleRng)

(1 to 10).foldLeft(simpleRng) { (rr, _) =>
  val (di, rrr) = doubleInt(rr)
  println(di.toString())
  rrr
}
