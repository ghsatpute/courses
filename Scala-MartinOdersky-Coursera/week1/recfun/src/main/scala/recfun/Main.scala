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
      if (c == 0 || c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      /**
        * Checks whether given string has matching parenthesis or not
        * @param chars string to be matched
        * @param count count of left parenthesis that to be matched
        * @return boolean value indicating whether string matches or not
        */
      def isBalanced(chars: List[Char], count : Int) : Boolean = {
        // If we've reached the end of array, and count is zero means we've matched all the parenthesis
        // If the count is not zero, that means there are unmatched parenthesis
        if(chars.isEmpty && count == 0)
          true
        else if(chars.isEmpty && count > 0)
          false
        else {
          // If current character is left parenthesis, increment the count and process the remaining string
          if (chars.head == '(')
            isBalanced(chars.tail, count + 1)
          // If current character is right parenthesis and there are unmatched left parenthesis then decrement the count
          // And process the remaining string
          else if (chars.head == ')' && count > 0)
            isBalanced(chars.tail, count - 1)
          // If current character is right parenthesis and there are not unmatched left parenthesis this is not going to
          // be a balanced string, return false
          else if (chars.head == ')' && count <= 0)
            false
          // If character is neither left parenthesis or right parenthesis forward the count and remaining string
          else
            isBalanced(chars.tail, count)
        }
      }
      isBalanced(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      // Base case,
      // If there are no coins then you can't make any money
      if(coins.length == 0 || money < 0)
        0;
      else if(money == 0)
        1;
      // There are two options either choose current coin or not
      else countChange(money, coins.tail) + // Don't chose this coin, continue with rest of coins
        countChange(money - coins.head, coins) // Chose this coin, decrease the money by the amount of coin value.
                                                //
    }
  }
