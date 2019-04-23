import com.dawidkubicki.fpis.chapter6.{RNG, SimpleRNG}
import com.dawidkubicki.fpis.chapter6.Util._

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
  fs.foldLeft((List.empty[A], rng)) { case ((acc, rngAcc), nextRand) =>
    val (newElem, updatedRng) = nextRand(rngAcc)
    (acc :+ newElem, updatedRng)
  }
}


val rng: RNG = SimpleRNG(427)

sequence(List.fill(10)(int))(rng)

sequence(List.fill(10)(nonNegativeInt _))(rng)

sequence(List.fill(10)(double _))(rng)
