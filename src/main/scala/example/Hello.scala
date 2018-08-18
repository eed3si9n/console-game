package example

import org.fusesource.jansi.{ AnsiConsole, Ansi }
import Ansi.{ ansi, Color }

object Hello extends App {
  val b0 = ansi().saveCursorPosition().eraseScreen()
  val b1 = drawbox(b0, 2, 6, 20, 10)
  val b2 = b1
    .bold
    .cursor(7, 10)
    .a("***")
    .cursor(8, 10)
    .a(" * ")
    .boldOff
    .restoreCursorPosition()
    .reset()

  AnsiConsole.out.println(b2)

  def drawbox(builder: Ansi, x0: Int, y0: Int, w: Int, h: Int): Ansi = {
    require(w > 1 && h > 1)
    val topStr = "┌".concat("─" * (w - 2)).concat("┐")
    val wallStr = "│".concat(" " * (w - 2)).concat("│")
    val bottomStr = "└".concat("─" * (w - 2)).concat("┘")
    val top = builder.cursor(y0, x0).a(topStr)
    val walls = (0 to h - 2).toList.foldLeft(top) { (b: Ansi, i: Int) =>
       b.cursor(y0 + i + 1, x0).a(wallStr)
     }
    walls.cursor(y0 + h - 1, x0).a(bottomStr)
  }
}
