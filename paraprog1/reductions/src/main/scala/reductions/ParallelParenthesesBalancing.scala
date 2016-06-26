package reductions

import scala.annotation._
import org.scalameter._
import common._

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
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    try {
      val depth = chars.foldLeft(0)((sum, c) => c match {
        case '(' => sum + 1
        case ')'if sum > 0 => sum - 1
        case ')'if sum <= 0 => throw new IllegalStateException("Mismatched parenthesis")
        case _ => sum
      })
    depth == 0      
    }
    catch {
      case _ : IllegalStateException => false
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    sealed case class Balance(openCount : Int, closeCount : Int)

    @tailrec
    def traverse(idx: Int, until: Int, leftCount: Int, rightCount: Int) : Balance = {
      if (idx>=until)
        Balance(leftCount, rightCount)
      else
        chars(idx) match {
        case '(' => traverse(idx + 1, until, leftCount + 1, rightCount)
        case ')' => traverse(idx + 1, until, leftCount, rightCount + 1)
        case _ => traverse(idx + 1, until, leftCount, rightCount)
      }
    }

    def reduce(from: Int, until: Int) : Balance = {
      if ((until - from) < threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val (leftOpenCloses, rightOpenCloses) = parallel(reduce(from, mid), reduce(mid, until))
        Balance(leftOpenCloses.openCount + rightOpenCloses.openCount, leftOpenCloses.closeCount + rightOpenCloses.closeCount) 
      }        
    }
    
    var bal = reduce(0, chars.length)
    bal.openCount == bal.closeCount
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
