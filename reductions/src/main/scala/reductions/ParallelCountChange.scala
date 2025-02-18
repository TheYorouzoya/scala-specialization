package reductions

import org.scalameter.*

object ParallelCountChangeRunner:

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns := 20,
    Key.exec.maxWarmupRuns := 40,
    Key.exec.benchRuns := 80,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    catch
      case e: NotImplementedError =>
        println("Not implemented.")

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))

object ParallelCountChange extends ParallelCountChangeInterface:

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def countChangeHelper(amount: Int, coinsLeft: List[Int], ways: Int): Int =
      if (coinsLeft.isEmpty || amount < 0) then ways
      else if (amount == 0) then ways + 1
      else if (coinsLeft.length == 1) then {
        if (amount % coinsLeft.head) == 0 then ways + 1 else ways
      }
      else
        val left_ways = countChangeHelper(amount - coinsLeft.head, coinsLeft, ways)
        countChangeHelper(amount, coinsLeft.tail, left_ways)      

    if money == 0 then 1 
    else countChangeHelper(money, coins, 0)

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int =
    def countHelper(amount: Int, coinsLeft: List[Int], ways: Int): Int =
      if (coinsLeft.isEmpty || amount < 0) then ways
      else if (coinsLeft.length == 1) then {
        if (amount % coinsLeft.head) == 0 then ways + 1 else ways
      }
      else
        if threshold(amount, coinsLeft) then countChange(amount, coinsLeft)
        else 
          val (left, right) = parallel(countHelper(amount - coinsLeft.head, coinsLeft, ways),
                                        countHelper(amount, coinsLeft.tail, ways))
          left + right
    
    if money == 0 then 1
    else countHelper(money, coins, 0)

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (amount: Int, _: List[Int]) => 
      amount <= ((startingMoney * 2) / 3)

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_: Int, coinsLeft: List[Int]) =>
      coinsLeft.length <= ((totalCoins * 2) / 3)


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    (amount: Int, coinsLeft: List[Int]) =>
      (amount * coinsLeft.length) <= ((startingMoney * allCoins.length) / 2)
