import com.dawidkubicki.fpis.chapter6.SimpleRNG
import com.dawidkubicki.fpis.chapter6.Util._

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
  val (a, ra2) = ra(rng)
  val (b, rb2) = rb(ra2)
  (f(a, b), rb2)
}

val rng = SimpleRNG(42)

val ra1 = nonNegativeInt _
val ra2 = double _

map2(ra1, ra2)(_ + _)(rng)

map2(ra1, ra2)((_, _))(rng)

map2(ra1, ra2)(_.toString + " " + _.toString)(rng)

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] = both(int, double)
val randDoubleInt: Rand[(Double, Int)] = both(double, int)