package example

import org.fusesource.jansi.{ AnsiConsole, Ansi }
import Ansi.{ ansi, Color }
import jline.console.{ ConsoleReader, KeyMap, Operation }
import java.io._
import scala.concurrent.{ Future, ExecutionContext }
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

object ConsoleGame extends App {
  val reader = new ConsoleReader()
  val isGameOn = new AtomicBoolean(true)
  val keyPressses = new ConcurrentLinkedQueue[Either[Operation, String]]()
  case class GameState(pos: (Int, Int))
  var gameState: GameState = GameState(pos = (6, 4))

  AnsiConsole.out.println(eraseScreen.run(ansi())._1)

  import ExecutionContext.Implicits._

  val inputHandling = Future {
    val km = KeyMap.keyMaps().get("vi-insert")
    while (isGameOn.get) {
      val c = reader.readBinding(km)
      val k: Either[Operation, String] =
        if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
        else Left(c match { case op: Operation => op })
      keyPressses.add(k)
    }
  }

  while (isGameOn.get) {
    while (!keyPressses.isEmpty) {
      Option(keyPressses.poll) foreach { k =>
        gameState = handleKeypress(k, gameState)
      }
    }
    drawGame(gameState)
    Thread.sleep(100)
  }

  def handleKeypress(k: Either[Operation, String], g: GameState): GameState =
    k match {
      case Right("q") =>
        isGameOn.set(false)
        g
      // Left arrow
      case Left(Operation.BACKWARD_CHAR) =>
        val pos0 = gameState.pos
        g.copy(pos = (pos0._1 - 1, pos0._2))
      // Right arrow
      case Left(Operation.FORWARD_CHAR) =>
        val pos0 = g.pos
        g.copy(pos = (pos0._1 + 1, pos0._2))
      // Down arrow
      case Left(Operation.NEXT_HISTORY) =>
        val pos0 = g.pos
        g.copy(pos = (pos0._1, pos0._2 + 1))
      // Up arrow
      case Left(Operation.PREVIOUS_HISTORY) =>
        g
      case _ =>
        // println(k)
        g
    }

  def drawGame(g: GameState): Unit = {
    val program: BuilderHelper[Ansi, Unit] =
      for {
        _ <- drawBox(2, 2, 20, 10)
        _ <- drawBlock(g.pos._1, g.pos._2)
        _ <- drawText(2, 12, "press 'q' to quit")
      } yield ()
    val result = program.run(ansi())._1
    AnsiConsole.out.println(result)
  }

  def eraseScreen: BuilderHelper[Ansi, Unit] = BuilderHelper.unit { b: Ansi =>
    b.eraseScreen()
  }

  def drawText(x: Int, y: Int, text: String): BuilderHelper[Ansi, Unit] = BuilderHelper.unit { b: Ansi =>
    b.cursor(y, x).a(text)
  }

  def drawBlock(x: Int, y: Int): BuilderHelper[Ansi, Unit] = BuilderHelper.unit { b: Ansi =>
    b.bold
      .cursor(y, x)
      .a("***")
      .cursor(y + 1, x)
      .a(" * ")
      .boldOff
  }

  def drawBox(x0: Int, y0: Int, w: Int, h: Int): BuilderHelper[Ansi, Unit] = BuilderHelper.unit { b: Ansi =>
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
}
