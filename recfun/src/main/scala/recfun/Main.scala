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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0) 1
    else if(c == r) pascal(c - 1, r - 1)
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(acc: Int, chars: List[Char]): Int = {
      if (acc < 0) -1
      else if (chars.isEmpty) acc
      else if (chars.head == '(') isBalanced(acc + 1, chars.tail)
      else if (chars.head == ')') isBalanced(acc - 1, chars.tail)
      else isBalanced(acc, chars.tail)
    }

    isBalanced(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0) 0
    else {
      def loop(acc: Int, list: List[Int]): Int = {
        if(list.isEmpty) acc
        else loop(acc + countChange(money - list.head, list), list.tail)
      }
      loop(0, coins)
    }
  }
}

