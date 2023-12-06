import scala.annotation.tailrec

//noinspection ScalaWeakerAccess
object MyApp extends App {

	val mapdata = ReadFile("data.txt")

	MainMenu()

	def ReadFile(filename: String): Map[String, List[Int]] = try {
		val source = scala.io.Source.fromFile(filename)
		val lines = source.getLines().toList
		source.close()

		lines.map { line =>
			val data = line.split(",").map(_.trim)
			val food = data.head
			val prices = data.tail.map(_.toInt).toList
			food -> prices
		}.toMap
	} catch {
		case ex: Exception =>
			println("File Manipulation Exception Occurred: " + ex)
			println("Exiting Application...")
			System.exit(0)
			Map.empty[String, List[Int]] // Return an empty map in case of an exception
		// Although this line is never hit due to the system.exit call, the compiler treats the system.exit as returning a unit
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

		val choice = scala.io.StdIn.readLine()

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
				println("Exiting Application...")
				System.exit(0)
			case _ => println("Incorrect Selection")
		}

		println("\r\n========\r\n")
		MainMenu()
	}

	//region Current Prices
	def DisplayCurrentPrices(foodData: Map[String, Int]): Unit = {
		foodData.foreach((food, price) => println(f"$food = £${price.toDouble / 100f}%1.2f"))
	}

	def GetCurrentPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {
		foodData.map { case (food, prices) => food -> prices.last }
	}
	//endregion

	//region Min Max
	def DisplayMinMax(foodData: Map[String, (Int, Int)]): Unit = {
		foodData.foreach((food, prices) => println(f"$food: Max = £${prices._1.toDouble / 100f}%1.2f | Min = £${prices._2.toDouble / 100f}%1.2f"))
	}

	def GetMinMaxPrices(foodData: Map[String, List[Int]]): Map[String, (Int, Int)] = {
		foodData.map { case (food, prices) => food -> (prices.max, prices.min) }
	}
	//endregion

	//region Median
	def DisplayMedian(foodData: Map[String, Int]): Unit = {
		foodData.foreach((food, price) => println(f"$food = £${price.toDouble / 100f}%1.2f"))
	}

	def GetMedianPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {
		foodData.map { case (food, prices) =>
			val sortedPrices = prices.sorted
			val median = if (prices.length % 2 == 0) {
				val middleIndex = prices.length / 2
				(sortedPrices(middleIndex - 1) + sortedPrices(middleIndex)) / 2
			} else {
				sortedPrices(prices.length / 2)
			}
			food -> median
		}
	}
	//endregion

	//region Rising Food
	def DisplayRising(foodData: Map[String, Int]): Unit = {
		val max = foodData.maxBy(_._2)

		if (max._2 < 1) {
			println("No Rise Found")
		}

		println(f"${max._1}: rose by £${max._2.toDouble / 100f}%1.2f")
	}

	def GetRisingFoods(foodData: Map[String, List[Int]]): Map[String, Int] = {
		foodData.map { case (food, prices) =>
			val sixMonths = prices.takeRight(6)
			val rise = if (sixMonths.length >= 2) sixMonths.last - sixMonths.head else 0
			food -> rise
		}
	}
	//endregion

	//region Avg
	def AvgMenu(foodData: Map[String, List[Int]]): Unit = {
		val foodNames = foodData.keys.toList

		DisplayAvgMenuOptions(foodNames, 1)

		val choice1 = GetMenuInput(foodNames.length) - 1
		val choice2 = GetMenuInput(foodNames.length) - 1

		val (food1, prices1) = foodNames(choice1) -> foodData.get(foodNames(choice1)).head
		val (food2, prices2) = foodNames(choice2) -> foodData.get(foodNames(choice2)).head
		val avgPrices = GetAveragePrices((food1, prices1), (food2, prices2)).splitAt(1)

		val dif = (avgPrices._1.head._2 - avgPrices._2.head._2).abs

		println(f"${avgPrices._1.head._1}: Avg = £${avgPrices._1.head._2.toDouble / 100f}%1.2f | ${avgPrices._2.head._1} " +
				f"Avg = £${avgPrices._2.head._2.toDouble / 100f}%1.2f | There is a difference of £${dif.toDouble / 100f}%1.2f")
	}

	def GetMenuInput(optionSize: Int): Int = {
		print("Enter your choice: ")
		val choice = scala.io.StdIn.readLine()

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
				GetMenuInput(optionSize)
		}
	}

	@tailrec
	def DisplayAvgMenuOptions(foodNames: List[String], i: Int): Unit = {
		if (foodNames.isEmpty) {
			return
		}

		println(f"$i. ${foodNames.head}")

		DisplayAvgMenuOptions(foodNames.tail, i + 1)
	}

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

		DisplayBasketMenuOptions(foodData - current._1, i + 1)
	}

	def GetBasketItems(foodData: Map[String, Int], selectedFoods: Map[String, (Int, Double)]): Map[String, (Int, Double)] = {
		val foodNames = foodData.keys.toList

		val selectedPos = GetMenuInput(foodData.size + 1) - 1

		if (selectedPos == foodData.size) {
			if (selectedFoods.isEmpty) {
				println("Basket is empty, add items to basket to checkout")
				return GetBasketItems(foodData, Map.empty[String, (Int, Double)])
			}
			return selectedFoods
		}

		val selectedItem = foodData.filter(f => f._1 == foodNames(selectedPos)).head

		val item = (selectedItem._1, (selectedItem._2, GetQuantityInput()))

		GetBasketItems(foodData, selectedFoods + item)
	}

	def CalculateBasketTotal(basket: Map[String, (Int, Double)]): Double = {
		val total: Double = basket.map { case (a, b) => b._1 * b._2 }.sum
		total / 100
	}
	//endregion

	def GetQuantityInput(): Double = {
		print("Enter the Amount (KG/L): ")
		val choice = scala.io.StdIn.readLine()

		try {
			val choiceDouble = choice.toDouble

			if (choiceDouble <= 0.00) {
				println("Quantity must be greater than 0.00")
				return GetQuantityInput()
			}

			choiceDouble

		}
		catch {
			case e: Exception =>
				println("Quantity must be a Double")
				GetQuantityInput()
		}
	}
}
