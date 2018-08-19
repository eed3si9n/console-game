package example

import org.fusesource.jansi.{ AnsiConsole, Ansi }
import jline.console.{ ConsoleReader, KeyMap, Operation }
import scala.concurrent.{ blocking, Future, ExecutionContext }
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ArrayBlockingQueue

object ConsoleGame extends App {
  val reader = new ConsoleReader()
  val isGameOn = new AtomicBoolean(true)
  val keyPressses = new ArrayBlockingQueue[Either[Operation, String]](128)
  case class GameState(pos: (Int, Int))
  var gameState: GameState = GameState(pos = (6, 4))

  AnsiConsole.out.println(Draw.eraseScreen.run(Ansi.ansi())._1)

  import ExecutionContext.Implicits._

  // inside a background thread
  val inputHandling = Future {
    val km = KeyMap.keyMaps().get("vi-insert")
    while (isGameOn.get) {
      blocking {
        val c = reader.readBinding(km)
        val k: Either[Operation, String] =
          if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
          else Left(c match { case op: Operation => op })
        keyPressses.add(k)
      }
    }
  }

  // inside the main thread
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
      case Right("q") | Left(Operation.VI_EOF_MAYBE) =>
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
    val drawing: BuilderHelper[Ansi, Unit] =
      for {
        _ <- Draw.drawBox(2, 2, 20, 10)
        _ <- Draw.drawBlock(g.pos._1, g.pos._2)
        _ <- Draw.drawText(2, 12, "press 'q' to quit")
      } yield ()
    val result = drawing.run(Ansi.ansi())._1
    AnsiConsole.out.println(result)
  }
}
