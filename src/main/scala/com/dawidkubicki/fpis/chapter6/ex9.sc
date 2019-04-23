import com.dawidkubicki.fpis.chapter6.SimpleRNG
import com.dawidkubicki.fpis.chapter6.Util._

import scala.util.Random

val simpleRNG = SimpleRNG(Random.nextInt())

def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
  flatMap(r)(x => unit(f(x)))

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(rb)(b => map(ra)(a => f(a, b)))

//  flatMap(rb)(b => flatMap(ra)(a => unit(f(a, b))))