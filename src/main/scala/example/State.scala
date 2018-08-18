package example

class State[S, A](val run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = {
    State[S, B] { s0: S =>
      val (s1, a) = run(s0)
      (s1, f(a))
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State[S, B] { s0: S =>
      val (s1, a) = run(s0)
      f(a).run(s1)
    }
  }
}

object State {
  def apply[S, A](run: S => (S, A)): State[S, A] = new State(run)
  def unit[S](run: S => S): State[S, Unit] = State(s0 => (run(s0), ()))
}
