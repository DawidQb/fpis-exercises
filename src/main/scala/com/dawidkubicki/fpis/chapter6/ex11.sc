import com.dawidkubicki.fpis.chapter6.State

sealed trait Input
case object Coin extends Input
case object Turn extends Input

sealed trait Result
case object NoMoreCandies extends Result
case object UnlockingMachine extends Result
case object LockedMachine extends Result
case object MachineAlreadyUnlocked extends Result
case object CandySold extends Result

case class Machine(locked: Boolean, candies: Int, coins: Int)

def doSomeInput(input: Input): State[Machine, Result] = State { s =>
  if (s.candies <= 0) (NoMoreCandies, s)
  else {
    input match {
      case Coin if s.locked => (UnlockingMachine, s.copy(locked = false, coins = s.coins + 1))
      case Coin if !s.locked => (MachineAlreadyUnlocked, s)
      case Turn if s.locked => (LockedMachine, s)
      case Turn if !s.locked => (CandySold, s.copy(locked = true, candies = s.candies - 1))
    }
  }
}

def doSomeInput2(input: Input): State[Machine, (Int, Int)] = State { s =>
  if (s.candies <= 0) ((s.candies, s.coins), s)
  else {
    input match {
      case Coin if s.locked => ((s.candies, s.coins + 1), s.copy(locked = false, coins = s.coins + 1))
      case Coin if !s.locked => ((s.candies, s.coins), s)
      case Turn if s.locked => ((s.candies, s.coins), s)
      case Turn if !s.locked => ((s.candies - 1, s.coins), s.copy(locked = true, candies = s.candies - 1))
    }
  }
}

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State.sequence(inputs.map(doSomeInput2)).map(_.last)

val startState = Machine(true, 10, 0)

simulateMachine(List(Coin, Turn, Turn, Turn, Coin, Coin, Turn, Coin)).run(startState)

simulateMachine(List(Coin, Turn, Turn, Turn, Coin, Coin, Turn, Coin)).run(Machine(true, 1, 0))

