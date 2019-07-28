package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = c match {
      case 0 => 1  // when index of column "c" is 0
      case c if (c >=r) => 1 // when index of column "c" is greater than or equal to index of row "r"
      case _ => (pascal(c-1,r-1) + pascal(c,r-1))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceCheck(chars: List[Char], countOpens: Int): Boolean =
        if (chars.isEmpty) countOpens == 0
        else {
          chars.head match {
            case '(' => balanceCheck(chars.tail, (countOpens+1))
            case ')' =>  if (countOpens > 0) balanceCheck(chars.tail, (countOpens-1))
            else false
            case _ => balanceCheck(chars.tail, countOpens)
          }
        }

      balanceCheck(chars, 0) //The counter "countOpens" is initialized to zero
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      money match {
        case 0  => 1 // when money is zero
        case m if (m < 0) => 0 // when money is less than zero
        case m if (m >= 1 && coins.isEmpty) => 0 // when money is greater than or equal to 1 and coins isEmpty
        case _ => (countChange(money, coins.tail) + countChange(money - coins.head, coins))

      }

    }
  }
