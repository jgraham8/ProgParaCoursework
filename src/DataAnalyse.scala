class DataAnalyse {
  // Analysis 1: Get current (most recent) price for each food
  def getCurrentPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {

    val foods = foodData.map { food =>
      val name = food._1
      val price = food._2.head
      name -> price
    }

    foods
  }

  // Analysis 2: Get the highest and lowest prices within the period for each food
  def getMinMaxPrices(foodData: Map[String, List[Int]]): Map[String, (Int, Int)] = {

    val foods = foodData.map { food =>
      val name = food._1
      val max = food._2.max
      val min = food._2.min
      name -> (max, min)
    }

    foods
  }

  // Analysis 3: Get the median price over the period for each food
  def getMedianPrices(foodData: Map[String, List[Int]]): Map[String, Int] = {

    val foods = foodData.map { food =>
      val name = food._1
      val median = food._2.splitAt(food._2.length / 2)._1.last
      name -> median
    }

    foods
  }

  // Analysis 4: Get the symbol for the food which has risen most over the last 6 months
  def getRisingFood(foodData: Map[String, List[Int]]): String = {


    "RISING_FOOD_SYMBOL"
  }

  // Analysis 5: Compare the average values over the 2-year period of two foods selected by the user
  def getAveragePrices(food1: (String, List[Int]), food2: (String, List[Int])): Map[String, Int] = {

    var foods: Map[String, Int] = Map()

    foods.++(food1._1 -> getAveragePrice(food1._2))
    foods.++(food2._1 -> getAveragePrice(food2._2))

    foods
  }

  def getAveragePrice(prices: List[Int]): Int = {
    val avg = prices.sum / prices.length

    avg
  }

  // Analysis 6: Allow the user to input a food basket and show its total value based on the current values
  def calculateBasketTotal(basket: Map[String, Double]): Double = {


    0.0 // Placeholder value
  }

}
