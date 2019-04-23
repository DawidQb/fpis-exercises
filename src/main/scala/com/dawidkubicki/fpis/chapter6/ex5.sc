import com.dawidkubicki.fpis.chapter6.Util._
import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG}

val rng: RNG = SimpleRNG(427)

def double: Rand[Double] =
  map(nonNegativeInt) {
    int => int.toDouble / (Int.MaxValue + 1L)
  }

(1 to 10).foldLeft(rng: RNG) { (rr, _) =>
  val (dd, rrr) = double(rr)
  println(dd)
  rrr
}