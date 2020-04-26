package org.pittlabs.rover

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ConsoleDriver extends App {

  type Step = Function3[Char, Board, Try[Rover], Try[Rover]]

  override def main(args: Array[String]): Unit = {
    val board = Board(100, 100)
    val rover = Rover(Point(10, 10), Direction.East)
    val actions = "fl"

    println(
      s"Start rover at point ${rover.point} with direction ${rover.direction}"
    )
    val result = actions.toCharArray.toStream.foldLeft(Try(rover)) {
      case (Success(rover), action)  => doAction(action, board, rover)
      case (failure @ Failure(_), _) => failure
    }

    result match {
      case Failure(exception) => println(s"Error. $exception")
      case Success(rover) =>
        println(
          s"Rover is in point ${rover.point} with direction ${rover.direction}"
        )
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

//   private def step(
//       action: Char
//   )(board: Board): Rover => Try[Rover] = {

//     val forward = { rover: Rover => RoverOperations.moveForward(rover)(board) }
//     val backward = { rover: Rover =>
//       RoverOperations.moveBackward(rover)(board)
//     }
//     val left = { rover: Rover => Success(RoverOperations.turnLeft(rover)) }
//     val right = { rover: Rover => Success(RoverOperations.turnRight(rover)) }
//     val failure = { _: Rover =>
//       Failure(
//         new RuntimeException(s"Unknown action '$action'")
//       )
//     }

//     action match {
//       case 'f' => forward
//       case 'b' => backward
//       case 'l' => left
//       case 'r' => right
//       case _   => failure
//     }
//   }
}
