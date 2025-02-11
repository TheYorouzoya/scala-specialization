package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c <= 0) || (c >= r) then 1 else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def check_bal(lst: List[Char], open : Int): Boolean =
      if (lst.isEmpty) 
        return open == 0
      
      val curr = lst.head
      
      if (curr != '(' && curr != ')')
        return check_bal(lst.tail, open)
      
      if (curr == '(')
        return check_bal(lst.tail, open + 1)

      if (open == 0) then return false else return check_bal(lst.tail, open - 1)

    check_bal(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    def countSubset(amount: Int, coins: List[Int], ways: Int): Int =

      if (coins.isEmpty || amount < 0)
        return ways
      
      if (amount == 0)
        return ways + 1

      if (coins.length == 1) {
        if (amount % coins.head) == 0 then return ways + 1 else return ways
      }

      val left_ways = countSubset(amount - coins.head, coins, ways)

      return countSubset(amount, coins.tail, left_ways)      

    countSubset(money, coins, 0)

    