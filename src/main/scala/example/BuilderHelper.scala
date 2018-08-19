package example

class BuilderHelper[S, A](val run: S => (S, A)) {
  def map[B](f: A => B): BuilderHelper[S, B] = {
    BuilderHelper[S, B] { s0: S =>
      val (s1, a) = run(s0)
      (s1, f(a))
    }
  }

  def flatMap[B](f: A => BuilderHelper[S, B]): BuilderHelper[S, B] = {
    BuilderHelper[S, B] { s0: S =>
      val (s1, a) = run(s0)
      f(a).run(s1)
    }
  }
}

object BuilderHelper {
  def apply[S, A](run: S => (S, A)): BuilderHelper[S, A] = new BuilderHelper(run)
  def unit[S](run: S => S): BuilderHelper[S, Unit] = BuilderHelper(s0 => (run(s0), ()))
}
