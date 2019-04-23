import com.dawidkubicki.fpis.chapter6.State

type Counter = Int

val state0: Counter = 0

val isItTrue = State[Counter, Boolean](counter => (true, counter + 1))

val checker = for {
  a <- isItTrue
  b <- isItTrue
  c <- isItTrue
} yield (a, b, c)

checker.run(state0)

val checker2 = for {
  _ <- isItTrue
  _ <- isItTrue
  _ <- isItTrue
} yield ()

checker2.run(state0)