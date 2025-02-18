package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    if chars.isEmpty then true
    else
      var index = 0
      var acc = 0
      val until = chars.length
      while (index < until) {
        if acc < 0 then return false
        else
          val curr = chars(index)
          if curr == '(' then acc = acc + 1
          else if curr == ')' then acc = acc - 1

          index = index + 1
      }
      acc == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var i = idx
      var currLeft = 0
      var currRight = 0
      while (i < until) {
        val curr = chars(i)
        if curr == '(' then currLeft = currLeft + 1
        else if curr == ')' then 
          if currLeft > 0 then currLeft = currLeft - 1
          else currRight = currRight + 1
    
        i = i + 1
      }
      (currLeft, currRight)
    }

    def reduce(from: Int, until: Int): (Int, Int)= {
      if until - from <= threshold then traverse(from, until)
      else
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid),
                                      reduce(mid, until))
        combine(left._1, right._1, left._2, right._2)
    }

    def combine(l1: Int, l2: Int, r1: Int, r2: Int): (Int, Int) =
      ((l2 + math.max(0, l1 - r2)), (r1 + math.max(0, r2 - l1)))

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

