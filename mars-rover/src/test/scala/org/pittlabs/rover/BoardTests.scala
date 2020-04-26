package org.pittlabs.rover

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class BoardTests extends FlatSpec with Matchers {
  val board = Board(10, 10)

  Seq(
    (Point(0, 0), true),
    (Point(0, 9), true),
    (Point(9, 0), true),
    (Point(9, 9), true),
    (Point(0, 10), false),
    (Point(10, 0), false),
    (Point(10, 10), false)
  ).foreach {
    case (point, expected) =>
      "isWithin" should s"return $expected for point $point" in {
        board.isWithin(point) shouldBe expected
      }
  }

  Seq(
    (Point(0, 10), Point(0, 0)),
    (Point(10, 0), Point(0, 0)),
    (Point(10, 10), Point(0, 0)),
    (Point(0, -1), Point(0, 9)),
    (Point(-1, 0), Point(9, 0)),
    (Point(-1, -1), Point(9, 9))
  ).foreach {
    case (point, expected) =>
      "wrap" should s"return $expected for point $point" in {
        board.wrap(point) shouldBe expected
      }
  }

  case class ExistsObstacleAtScenario(
      val point: Point,
      val expected: Boolean,
      val obstacles: Set[Point] = Set()
  )

  Seq(
    ExistsObstacleAtScenario(Point(0, 0), false),
    ExistsObstacleAtScenario(Point(0, 0), true, Set(Point(0, 0))),
    ExistsObstacleAtScenario(Point(0, 0), false, Set(Point(1, 1)))
  ).foreach {
    case ExistsObstacleAtScenario(point, expected, obstacles) =>
      "existsObstacleAt" should s"return $expected at point $point with obstacles $obstacles" in {
        val board = Board(10, 10, obstacles)
        board.existObstacleAt(point) shouldBe expected
      }
  }
}
