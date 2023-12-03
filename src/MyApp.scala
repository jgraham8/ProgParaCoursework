import scala.annotation.tailrec

//noinspection ScalaWeakerAccess
object MyApp extends App {

  val mapdata = ReadFile("data.txt")

  MainMenu()

  def ReadFile(filename: String): Map[String, List[Int]] = {
    var buffer: Map[String, List[Int]] = Map()
    try {
      val source = scala.io.Source.fromFile("data.txt")
      val lines = source.getLines().toList
      source.close()

      buffer = lines.map { line =>
        val data = line.split(",").map(_.trim)
        val food = data.head
        val prices = data.tail.map(_.toInt).toList
        food -> prices
      }.toMap
    } catch {
      case ex: Exception => println("File Manipulation Exception Occurred: " + ex)
    }
    buffer
  }

  @tailrec
  def MainMenu(): Unit = {
    println("1. Get current price for each food")
    println("2. Get highest and lowest prices for each food")
    println("3. Get median price for each food")
    println("4. Get the symbol for the food which has risen most over the last 6 months")
    println("5. Compare average values of two foods")
    println("6. Calculate basket total")
    println("7. Quit")
    print("Enter your choice: ")

    var choice = ""

    choice = scala.io.StdIn.readLine()

    println("\r\n--------\r\n")

    choice match {
      case "1" =>
        println("Current Prices\r\n")
        DisplayCurrentPrices(GetCurrentPrices(mapdata))
      case "2" =>
        println("Min Max Prices\r\n")
        DisplayMinMax(GetMinMaxPrices(mapdata))
      case "3" =>
        println("Median Prices\r\n")
        DisplayMedian(GetMedianPrices(mapdata))
      case "4" =>
        println("Rising Food\r\n")
        DisplayRising(GetRisingFoods(mapdata))
      case "5" =>
        println("Compare Average\r\n")
        AvgMenu(mapdata)
      case "6" =>
        println("Basket Menu\r\n")
        BasketMenu(GetCurrentPrices(mapdata))
      case "7" =>
        println("Exiting...")
        System.exit(0)
      case _ => println("Incorrect Selection")
    }

    println("\r\n========\r\n")
    MainMenu()
  }

  //region Current Prices
  @tailrec
  def DisplayCurrentPrices(foodData: Map[String, Int]): Unit = {
    if (foodData.isEmpty) {
      return
    }

    val current = foodData.head

    println(f"${current._1} = £${current._2.toFloat / 100f}%1.2f")

    DisplayCurrentPrices(foodData.filterNot(f => f._1 == current._1))
  }

  def GetCurrentPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {

    val foods = foodData.map { food =>
      val name = food._1
      val price = food._2.last
      name -> price
    }

    foods
  }
  //endregion

  //region Min Max
  @tailrec
  def DisplayMinMax(foodData : Map[String, (Int, Int)]): Unit = {
    if (foodData.isEmpty) {
      return
    }

    val current = foodData.head

    println(f"${current._1}: Max = £${current._2._1.toFloat / 100f}%1.2f | Min = £${current._2._2.toFloat / 100f}%1.2f")

    DisplayMinMax(foodData.filterNot(f => f._1 == current._1))
  }

  // Analysis 2: Get the highest and lowest prices within the period for each food
  def GetMinMaxPrices(foodData: Map[String, List[Int]]): Map[String, (Int, Int)] = {

    val foods = foodData.map { food =>
      val name = food._1
      val max = food._2.max
      val min = food._2.min
      name -> (max, min)
    }

    foods
  }
  //endregion

  //region Median
  @tailrec
  def DisplayMedian(foodData: Map[String, Int]): Unit = {
    if (foodData.isEmpty) {
      return
    }

    val current = foodData.head

    println(f"${current._1} = £${current._2.toFloat / 100f}%1.2f")

    DisplayMedian(foodData.filterNot(f => f._1 == current._1))
  }
  // Analysis 3: Get the median price over the period for each food
  def GetMedianPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {

    val foods = foodData.map { food =>
      val name = food._1
      val median = food._2.sorted.splitAt(food._2.length / 2)._1.last
      name -> median
    }

    foods
  }
  //endregion

  //region Rising Food
  def DisplayRising(foodData: Map[String, Int]): Unit = {

    val max = foodData.maxBy(_._2)

    if (max._2 < 0){
      println("No Rise Found")
    }

    println(f"${max._1}: rose by £${max._2.toFloat / 100f}%1.2f")
  }
  // Analysis 4: Get the symbol for the food which has risen most over the last 6 months
  def GetRisingFoods(foodData: Map[String, List[Int]]): Map[String, Int] = {

    val foods = foodData.map { food =>
      val name = food._1

      val year = food._2.splitAt(food._2.length / 2)._2
      val sixmonths = year.splitAt(year.length / 2)._2

      val rise = sixmonths.last - sixmonths.head

      name -> rise
    }

    foods
  }
  //endregion

  //region Avg
  def AvgMenu(foodData: Map[String, List[Int]]): Unit = {
    val foodnames = foodData.keys.toList

    DisplayAvgMenuOptions(foodnames, 1)

    val choice1 = GetMenuInput(foodnames.length) - 1

    DisplayAvgMenuOptions(foodnames, 1)

    val choice2 = GetMenuInput(foodnames.length) - 1

    val choice1Tuple: (String, List[Int]) = foodnames(choice1) -> foodData.get(foodnames(choice1)).head
    val choice2Tuple: (String, List[Int]) = foodnames(choice2) -> foodData.get(foodnames(choice2)).head
    val avgPrices = GetAveragePrices(choice1Tuple, choice2Tuple).splitAt(1)

    val dif = (avgPrices._1.head._2 - avgPrices._2.head._2).abs

    println(f"${avgPrices._1.head._1}: Avg = £${avgPrices._1.head._2.toFloat / 100f}%1.2f | ${avgPrices._2.head._1} Avg = £${avgPrices._2.head._2.toFloat / 100f}%1.2f | There is a difference of £${dif.toFloat / 100f}%1.2f")
  }

  @tailrec
  def DisplayAvgMenuOptions(foodNames: List[String], i: Int): Unit = {
    if (foodNames.isEmpty) {
      return
    }

    val current = foodNames.head

    println(f"$i. $current")

    DisplayAvgMenuOptions(foodNames.filterNot(f => f == current), i+1)
  }

  // Analysis 5: Compare the average values over the 2-year period of two foods selected by the user
  def GetAveragePrices(food1: (String, List[Int]), food2: (String, List[Int])): Map[String, Int] = {
    Map(food1._1 -> GetAveragePrice(food1._2), food2._1 -> GetAveragePrice(food2._2))
  }

  def GetAveragePrice(prices: List[Int]): Int = {
    prices.sum / prices.length
  }
  //endregion

  //region Item Basket
  def BasketMenu(foodData: Map[String, Int]): Unit = {

  }

  @tailrec
  def DisplayBasketMenuOptions(foodData: Map[String, Int], i: Int): Unit = {
    if (foodData.isEmpty) {
      return
    }

    val current = foodData.head

    println(f"$i. $current")

    DisplayBasketMenuOptions(foodData.filterNot(f => f == current), i + 1)
  }

  // Analysis 6: Allow the user to input a food basket and show its total value based on the current values
  def CalculateBasketTotal(basket: Map[String, Double]): Double = {


    0.0 // Placeholder value
  }
  //endregion


  def GetMenuInput(optionSize: Int): Int = {
    var choice = ""
    print("Enter your choice: ")
    choice = scala.io.StdIn.readLine()

    try {
      val choiceInt = choice.toInt

      if (choiceInt > optionSize || choiceInt < 1) {
        println("Incorrect Selection")
        GetMenuInput(optionSize)
      }

      choiceInt

    }
    catch {
      case e: Exception =>
        println("Incorrect Selection")
        GetMenuInput(optionSize)
    }
  }
}
