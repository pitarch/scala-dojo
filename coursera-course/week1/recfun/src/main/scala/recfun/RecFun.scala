package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = {

    @tailrec
    def visit(acc: Int, pending: List[(Int, Int)]): Int = {
      if (pending.isEmpty) return acc
      val (c, r) = pending.head

      var incrPending = List[(Int, Int)]()
      var incrAcc = 0
      if (c == 0 || c == r) incrAcc = 1
      else if (c == 1 || c == r - 1) incrAcc = r
      else incrPending = List((c, r - 1), (c - 1, r - 1))

      visit(acc + incrAcc, pending.tail ++ incrPending)
    }

    visit(0, List((c, r)))
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def visit(opened: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) return opened == 0

      var incr = 0
      if (chars.head == ')' && opened == 0) return false
      if (chars.head == ')') incr = -1
      if (chars.head == '(') incr = 1
      visit(opened + incr, chars.tail)
    }

    visit(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def sum(change: List[Int]): Int =
      change.zip(coins).map { case (a, b) => a * b }.sum

    case class Context(index: Int, change: List[Int])

    def incr(change: List[Int], index: Int, qnt: Int = 1): List[Int] =
      change.zipWithIndex.iterator.map { case (i, i1) => if (i1 == index) i + qnt else i }.toList

    @scala.annotation.tailrec
    def howManyChanges(acc: Int, contexts: List[Context], coins: List[Int]): Int = {

      var incrAcc = 0
      if (contexts.isEmpty) return acc
      var nextContexts = contexts.tail
      val context = contexts.head
      val total = sum(context.change)
      if (total == money) incrAcc = 1
      else if (total < money) {
        nextContexts = Context(context.index, incr(context.change, context.index)) +: nextContexts
        if (context.index + 1 < context.change.length) {
          nextContexts = nextContexts :+ Context(context.index + 1, context.change)
        }
      }

      howManyChanges(acc + incrAcc, nextContexts, coins)
    }

    val initalChangeSeed = List.fill(coins.length)(0)
    howManyChanges(0, List(Context(0, initalChangeSeed)), coins)
  }
}
