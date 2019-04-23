import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG, Util}

val rng = SimpleRNG(427)

def double(rng: RNG): (Double, RNG) = {
  val (n, r) = Util.nonNegativeInt(rng)
  val d = n.toDouble / (Int.MaxValue.toLong + 1)
  (d, r)
}

(1 to 10).foldLeft(rng: RNG) { (rr, _) =>
  val (dd, rrr) = double(rr)
  println(dd)
  rrr
}