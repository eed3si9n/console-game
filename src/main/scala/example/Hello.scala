package example

import org.fusesource.jansi.{ AnsiConsole, Ansi }
import Ansi.{ ansi, Color }

object Hello extends App {

  val program: State[Ansi, Unit] =
    for {
      _ <- saveCursorPosition
      _ <- drawBox(2, 6, 20, 10)
      _ <- drawBlock(6, 8)
      _ <- restoreCursorPosition
    } yield ()

  val result = program.run(ansi())._1
  AnsiConsole.out.println(result)

  def saveCursorPosition: State[Ansi, Unit] = State.unit { b: Ansi =>
    b.saveCursorPosition().eraseScreen()
  }

  def drawBlock(x: Int, y: Int): State[Ansi, Unit] = State.unit { b: Ansi =>
    b.bold
      .cursor(y, x)
      .a("***")
      .cursor(y + 1, x)
      .a(" * ")
      .boldOff
  }

  def drawBox(x0: Int, y0: Int, w: Int, h: Int): State[Ansi, Unit] = State.unit { b: Ansi =>
    require(w > 1 && h > 1)
    val topStr = "┌".concat("─" * (w - 2)).concat("┐")
    val wallStr = "│".concat(" " * (w - 2)).concat("│")
    val bottomStr = "└".concat("─" * (w - 2)).concat("┘")
    val top = b.cursor(y0, x0).a(topStr)
    val walls = (0 to h - 2).toList.foldLeft(top) { (bb: Ansi, i: Int) =>
       bb.cursor(y0 + i + 1, x0).a(wallStr)
     }
    walls.cursor(y0 + h - 1, x0).a(bottomStr)
  }

  def restoreCursorPosition: State[Ansi, Unit] = State.unit { b: Ansi =>
    b.restoreCursorPosition.reset()
  }
}
