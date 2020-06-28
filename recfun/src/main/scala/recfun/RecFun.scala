package recfun

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
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def tryMatch(remaining: List[Char], openCnt: Int): Boolean = remaining match {
      case '(' :: xs => tryMatch(xs, openCnt + 1)
      case ')' :: xs => {
        if (openCnt > 0) tryMatch(xs, openCnt - 1)
        else false
      }
      case _ :: xs => tryMatch(xs, openCnt)
      case Nil => if (openCnt > 0) false else true
    }

    tryMatch(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sortWith(_ > _)

    def doChange(curMoney: Int, curCoins: List[Int]): Int = {
      if (curMoney == 0) 1 else curCoins match {
        case x :: xs => {
          if (x <= curMoney) doChange(curMoney - x, curCoins) + doChange(curMoney, xs)
          else doChange(curMoney, xs)
        }
        case Nil => 0
      }
    }
    doChange(money, sortedCoins)
  }
}
