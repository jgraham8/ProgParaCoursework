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

    println(f"${current._1} = £${current._2.toDouble / 100f}%1.2f")

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

    println(f"${current._1}: Max = £${current._2._1.toDouble / 100f}%1.2f | Min = £${current._2._2.toDouble / 100f}%1.2f")

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

    println(f"${current._1} = £${current._2.toDouble / 100f}%1.2f")

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

    println(f"${max._1}: rose by £${max._2.toDouble / 100f}%1.2f")
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
    val foodNames = foodData.keys.toList

    DisplayAvgMenuOptions(foodNames, 1)

    val choice1 = GetMenuInput(foodNames.length) - 1

    DisplayAvgMenuOptions(foodNames, 1)

    val choice2 = GetMenuInput(foodNames.length) - 1

    val choice1Tuple: (String, List[Int]) = foodNames(choice1) -> foodData.get(foodNames(choice1)).head
    val choice2Tuple: (String, List[Int]) = foodNames(choice2) -> foodData.get(foodNames(choice2)).head
    val avgPrices = GetAveragePrices(choice1Tuple, choice2Tuple).splitAt(1)

    val dif = (avgPrices._1.head._2 - avgPrices._2.head._2).abs

    println(f"${avgPrices._1.head._1}: Avg = £${avgPrices._1.head._2.toDouble / 100f}%1.2f | ${avgPrices._2.head._1} " +
            f"Avg = £${avgPrices._2.head._2.toDouble / 100f}%1.2f | There is a difference of £${dif.toDouble / 100f}%1.2f")
  }

  def GetMenuInput(optionSize: Int): Int = {
    var choice = ""
    print("Enter your choice: ")
    choice = scala.io.StdIn.readLine()

    try {
      val choiceInt = choice.toInt

      if (choiceInt > optionSize || choiceInt < 1) {
        println(s"Selection must be between 1 - $optionSize")
        return GetMenuInput(optionSize)
      }

      choiceInt

    }
    catch {
      case e: Exception =>
        println("Selection must be an integer")
        return GetMenuInput(optionSize)
    }
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
    DisplayBasketMenuOptions(foodData, 1)

    val items = GetBasketItems(foodData, Map.empty[String, (Int, Double)])

    println(f"Total is £${CalculateBasketTotal(items)}%1.2f")
  }

  @tailrec
  def DisplayBasketMenuOptions(foodData: Map[String, Int], i: Int): Unit = {
    if (foodData.isEmpty) {
      println(f"$i. Checkout")
      return
    }

    val current = foodData.head

    println(f"$i. ${current._1} £${current._2.toDouble / 100f}%1.2f")

    DisplayBasketMenuOptions(foodData.filterNot(f => f == current), i + 1)
  }

  @tailrec
  def GetBasketItems(foodData: Map[String, Int], selectedFoods: Map[String, (Int, Double)]): Map[String, (Int, Double)] = {
    val foodNames = foodData.keys.toList

    var selectedPos = GetMenuInput(foodData.size + 1) - 1

    if (selectedPos == foodData.size){
      if (selectedFoods.isEmpty){
        println("Basket is empty, add items to basket to checkout")
        selectedPos = GetMenuInput(foodData.size + 1)
      }
      return selectedFoods
    }

    val selectedItem = foodData.filter(f => f._1 == foodNames(selectedPos)).head

    val item = (selectedItem._1, (selectedItem._2, GetQuantityInput()))

    GetBasketItems(foodData, selectedFoods + item)
  }

  // Analysis 6: Allow the user to input a food basket and show its total value based on the current values
  def CalculateBasketTotal(basket: Map[String, (Int, Double)]): Double = {
    var total: Double = 0.00
    basket.foreach(f => total = total + (f._2._2 * f._2._1).toDouble)

    total / 100
  }
  //endregion

  def GetQuantityInput(): Double = {
    var choice = ""
    print("Enter the Amount (KG/L): ")
    choice = scala.io.StdIn.readLine()

    try {
      val choiceDouble = choice.toDouble

      if (choiceDouble <= 0.00) {
        println("Quantity must be greater than 0.00")
        GetQuantityInput()
      }

      choiceDouble

    }
    catch {
      case e: Exception =>
        println("Quantity must be a Double ")
        GetQuantityInput()
    }
  }
}
