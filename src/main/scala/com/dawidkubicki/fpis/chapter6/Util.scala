package com.dawidkubicki.fpis.chapter6

object Util {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val itsAThree: Rand[Int] = x => (3, x)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, ra2) = ra(rng)
    val (b, rb2) = rb(ra2)
    (f(a, b), rb2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val nn =
      if (n > 0) n
      else if (n == Int.MinValue) 0
      else -n
    (nn, rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = Util.nonNegativeInt(rng)
    val d = n.toDouble / (Int.MaxValue.toLong + 1)
    (d, rng2)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}
