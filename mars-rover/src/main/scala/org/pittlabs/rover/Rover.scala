package org.pittlabs.rover

import scala.util.Success
import scala.util.Try
import scala.util.Failure

object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West = Value
}
import Direction._

case class Point(x: Int, y: Int)

case class Board(sizeX: Int, sizeY: Int, obstacles: Set[Point] = Set()) {

  def isWithin(point: Point): Boolean =
    point.x >= 0 && point.x < sizeX && point.y >= 0 && point.y < sizeY

  def wrap(point: Point): Point = {
    var x = point.x % sizeX
    var y = point.y % sizeY
    if (x < 0) x = sizeX + x
    if (y < 0) y = sizeY + y
    return Point(x, y)
  }

  def existObstacleAt(point: Point): Boolean = obstacles.contains(point)
}

case class Rover(point: Point, direction: Direction)

sealed abstract class RoverException extends RuntimeException
class BlockedByObstacleException(val rover: Rover, val obstacle: Point)
    extends RoverException

object RoverOperations {

  def moveForward(rover: Rover)(board: Board): Try[Rover] = {

    var nextPoint = rover.direction match {
      case North => Point(rover.point.x, rover.point.y + 1)
      case South => Point(rover.point.x, rover.point.y - 1)
      case East  => Point(rover.point.x + 1, rover.point.y)
      case West  => Point(rover.point.x - 1, rover.point.y)
    }

    if (board.existObstacleAt(nextPoint))
      return Failure(new BlockedByObstacleException(rover, nextPoint))
    else
      println(
        s"no obstance on ${nextPoint} when rover moving from ${rover.point}"
      )

    if (!board.isWithin(nextPoint)) nextPoint = board.wrap(nextPoint)

    Success(Rover(nextPoint, rover.direction))
  }

  def moveBackward(rover: Rover)(board: Board): Try[Rover] = {

    var nextPoint = rover.direction match {
      case North => Point(rover.point.x, rover.point.y - 1)
      case South => Point(rover.point.x, rover.point.y + 1)
      case East  => Point(rover.point.x - 1, rover.point.y)
      case West  => Point(rover.point.x + 1, rover.point.y)
    }

    if (board.existObstacleAt(nextPoint))
      return Failure(new BlockedByObstacleException(rover, nextPoint))
    if (!board.isWithin(nextPoint)) nextPoint = board.wrap(nextPoint)
    Success(Rover(nextPoint, rover.direction))
  }

  def turnLeft(rover: Rover): Rover = {

    val nextDirection = rover.direction match {
      case North => West
      case West  => South
      case South => East
      case East  => North
    }
    return Rover(rover.point, nextDirection)
  }

  def turnRight(rover: Rover): Rover = {

    val nextDirection = rover.direction match {
      case North => East
      case East  => South
      case South => West
      case West  => North
    }
    return Rover(rover.point, nextDirection)
  }
}
