package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
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
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def helper(idx: Int, open: Int): Boolean = {
      if (idx >= chars.length) {
        open == 0
      } else if (chars(idx) == '(') {
        helper(idx + 1, open + 1)
      } else if (chars(idx) == ')') {
        if (open > 0) {
          helper(idx + 1, open - 1)
        } else {
          false
        }
      } else {
        helper(idx + 1, open)
      }
    }
    helper(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx < chars.length && idx < until) {
        if (chars(idx) == '(') {
          traverse(idx + 1, until, arg1, arg2 + 1)
        } else if (chars(idx) == ')') {
          traverse(idx + 1, until, arg1 + 1, arg2)
        } else {
          traverse(idx + 1, until, arg1, arg2)
        }
      } else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from > threshold) {
        val mid = (until - from) / 2 + from
        val ((s1, e1), (s2, e2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (e1 < s2) {
          (s1 + s2 - e1, e2)
        } else {
          (s1, e1 - s2 + e2)
        }
      } else {
        traverse(from, until, 0, 0)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
