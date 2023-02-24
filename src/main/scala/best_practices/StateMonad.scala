package best_practices

import cats.data.State

object StateMonad {

  // sequential functional state = state monad
  // S => (S, A)

  case class Count(int: Int)
  val nextCount: State[Count, Int] = State { s =>
    val increment = 1

    (s.copy(s.int + increment), increment)
  }

  def seq: State[Count, Int] = for {
    n1 <- nextCount
    n2 <- nextCount
    n3 <- nextCount
  } yield (n1 + n2 + n3) // sum up ints

  // will not work in shared state situation
  // will only work in sequential computation

}
