import com.dawidkubicki.fpis.chapter6.SimpleRNG

val rng = SimpleRNG(SimpleRNG(42).nextInt._1)

val (n1, rng2) = rng.nextInt
val (n11, rng21) = rng.nextInt

val (n2, rng3) = rng2.nextInt