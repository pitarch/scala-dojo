package org.pittlabs.rover

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ConsoleDriver extends App {

  type Step = Function3[Char, Board, Try[Rover], Try[Rover]]

  // -b0,0 -r0,0 -o0,0;1,1
  override def main(args: Array[String]): Unit = {
    val board = Board(100, 100)
    val rover = Rover(Point(10, 10), Direction.East)
    val actions = "fl"

    println(
      s"Start rover at point ${rover.point} with direction ${rover.direction}"
    )
    val result = RoverActionManager.performActions(actions.toSeq, rover, board)

    result match {
      case Failure(exception) => println(s"Error. $exception")
      case Success(rover) =>
        println(
          s"Rover is in point ${rover.point} with direction ${rover.direction}"
        )
    }
  }
}

object RoverActionManager {

  def performActions(
      actions: Seq[Char],
      rover: Rover,
      board: Board
  ): Try[Rover] = {
    actions.foldLeft(Try(rover)) {
      case (Success(rover), action)  => doAction(action, board, rover)
      case (failure @ Failure(_), _) => failure
    }
  }

  private def mayBeDoAction(
      action: Char,
      board: Board,
      maybeRover: Try[Rover]
  ) =
    maybeRover match {
      case Success(rover)       => doAction(action, board, rover)
      case failure @ Failure(_) => failure
    }

  private def doAction(action: Char, board: Board, rover: Rover): Try[Rover] = {

    action match {
      case 'f' => RoverOperations.moveForward(rover)(board)
      case 'b' =>
        RoverOperations.moveBackward(rover)(board)
      case 'l' =>
        Success(RoverOperations.turnLeft(rover))
      case 'r' =>
        Success(RoverOperations.turnRight(rover))
      case _ => Failure(new RuntimeException(s"Unknown action '$action'"))
    }
  }
}

object ConfigByArgsBuilder {

  //-b0,0 -r0,0 -o0,0;1,1
  def build(args: Array[String]): Either[String, (Board, Rover)] = {

    val boardDef = args.filter(_.startsWith(("-b")))
    val obstaclesDef = args.filter(_.startsWith(("-o")))
    val roverDef = args.filter(_.startsWith(("-r")))

    if (boardDef.length == 0) return Left("Missing Board definition")
    if (boardDef.length > 1) return Left("Multiple Board definitions")
    if (roverDef.length == 0) return Left("Missing rover definition")

    Left("")
  }

}
