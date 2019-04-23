package com.dawidkubicki.fpis.chapter6

final case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s: S =>
    val (a, s2) = run(s)
    (f(a), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { s: S =>
    fs.foldLeft((List.empty[A], s)) { case ((listAcc, stateAcc), nextState) =>
      val (newElem, updatedState) = nextState.run(stateAcc)
      (listAcc :+ newElem, updatedState)
    }
  }

}