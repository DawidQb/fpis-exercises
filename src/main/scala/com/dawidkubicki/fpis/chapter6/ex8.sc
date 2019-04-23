import com.dawidkubicki.fpis.chapter6.SimpleRNG
import com.dawidkubicki.fpis.chapter6.Util._

import scala.util.Random

val simpleRNG = SimpleRNG(Random.nextInt())

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
  val (a, rng2) = f(rng)
  g(a)(rng2)
}


def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else {
      println("Olaboga")
      nonNegativeLessThan(n)
    }
  }

nonNegativeLessThan(3)(simpleRNG)

nonNegativeLessThan(999999)(simpleRNG)

