package org.pittlabs.rover
import org.scalatest._

class RoverOperationsTests extends FlatSpec with Matchers with TryValues {

  import RoverOperations._
  import Direction._

  case class MovementExpectation(
      initialPoint: Point,
      nextPoint: Point,
      direction: Direction
  )

  case class BlockedMovementExpectation(
      initialPoint: Point,
      direction: Direction,
      obstacle: Point
  )

  Seq(
    MovementExpectation(Point(1, 1), Point(1, 2), North),
    MovementExpectation(Point(1, 1), Point(1, 0), South),
    MovementExpectation(Point(1, 1), Point(0, 1), West),
    MovementExpectation(Point(1, 1), Point(2, 1), East)
  ).foreach { scenario =>
    "moveForward" should s"move rover from ${scenario.initialPoint} to ${scenario.nextPoint} when facing ${scenario.direction}" in {

      val board = Board(10, 10)
      val rover = Rover(scenario.initialPoint, scenario.direction)
      val result = moveForward(rover)(board)

      result.success.value.point shouldBe scenario.nextPoint
    }
  }

  Seq(
    MovementExpectation(Point(1, 1), Point(1, 0), North),
    MovementExpectation(Point(1, 1), Point(1, 2), South),
    MovementExpectation(Point(1, 1), Point(2, 1), West),
    MovementExpectation(Point(1, 1), Point(0, 1), East)
  ).foreach { scenario =>
    "moveBackward" should s"move rover from ${scenario.initialPoint} to ${scenario.nextPoint} when facing ${scenario.direction}" in {

      val board = Board(10, 10)
      val rover = Rover(scenario.initialPoint, scenario.direction)
      val result = moveBackward(rover)(board)

      result.success.value.point shouldBe scenario.nextPoint
    }
  }

  Seq(North, South, East, West).foreach { direction =>
    "moveForward" should s"keep facing '$direction' after moving" in {

      val board = Board(10, 10)
      val rover = Rover(Point(1, 1), direction)
      val result = moveForward(rover)(board)

      result.success.value.direction shouldBe direction
    }
  }

  Seq(North, South, East, West).foreach { direction =>
    "moveBackward" should s"keep facing '$direction' after moving" in {

      val board = Board(10, 10)
      val rover = Rover(Point(1, 1), direction)
      val result = moveBackward(rover)(board)

      result.success.value.direction shouldBe direction
    }
  }

  Map(North -> West, West -> South, South -> East, East -> North).foreach {
    case (direction, nextDirection) => {
      "turnLeft" should s"change direction from $direction to $nextDirection" in {
        val rover = Rover(Point(1, 1), direction)
        val nextRover = turnLeft(rover)
        nextRover.direction shouldBe nextDirection
      }
    }
  }

  Seq(North, South, East, West).foreach { direction =>
    "turnLeft" should s"not change the position when facing $direction" in {

      val rover = Rover(Point(1, 1), direction)
      val nextRover = turnLeft(rover)
      nextRover.point shouldBe rover.point
    }
  }

  Map(North -> East, West -> North, South -> West, East -> South).foreach {
    case (direction, nextDirection) => {
      "turnRight" should s"change direction from $direction to $nextDirection" in {
        val rover = Rover(Point(1, 1), direction)
        val nextRover = turnRight(rover)
        nextRover.direction shouldBe nextDirection
      }
    }
  }

  Seq(North, South, East, West).foreach { direction =>
    "turnRight" should s"not change the position when facing $direction" in {

      val rover = Rover(Point(1, 1), direction)
      val nextRover = turnRight(rover)
      nextRover.point shouldBe rover.point
    }
  }

  Seq(
    MovementExpectation(Point(0, 9), Point(0, 0), North),
    MovementExpectation(Point(1, 0), Point(1, 9), South),
    MovementExpectation(Point(0, 1), Point(9, 1), West),
    MovementExpectation(Point(9, 1), Point(0, 1), East)
  ).foreach { scenario =>
    "moveForward" should s"wrap the rover from ${scenario.initialPoint} to ${scenario.nextPoint} when facing ${scenario.direction}" in {

      val board = Board(10, 10)
      val rover = Rover(scenario.initialPoint, scenario.direction)
      val result = moveForward(rover)(board)

      result.success.value.point shouldBe scenario.nextPoint
    }
  }

  Seq(
    MovementExpectation(Point(0, 9), Point(0, 0), South),
    MovementExpectation(Point(1, 0), Point(1, 9), North),
    MovementExpectation(Point(0, 1), Point(9, 1), East),
    MovementExpectation(Point(9, 1), Point(0, 1), West)
  ).foreach { scenario =>
    "moveBackward" should s"wrap the rover from ${scenario.initialPoint} to ${scenario.nextPoint} when facing ${scenario.direction}" in {

      val board = Board(10, 10)
      val rover = Rover(scenario.initialPoint, scenario.direction)
      val result = moveBackward(rover)(board)

      result.success.value.point shouldBe scenario.nextPoint
    }
  }

  Seq(
    BlockedMovementExpectation(Point(1, 1), East, Point(2, 1)),
    BlockedMovementExpectation(Point(1, 1), North, Point(1, 2)),
    BlockedMovementExpectation(Point(1, 1), West, Point(0, 1)),
    BlockedMovementExpectation(Point(1, 1), South, Point(1, 0))
  ).foreach { scenario =>
    "moveForward" should s"get rover blocked when moving from ${scenario.initialPoint} with direction ${scenario.direction} with an obstacle in point ${scenario.obstacle}" in {

      val board = Board(10, 10, Set(scenario.obstacle))
      val rover = Rover(scenario.initialPoint, scenario.direction)
      val result = moveForward(rover)(board)

      result.failure.exception shouldBe a[BlockedByObstacleException]
    }
  }
}
