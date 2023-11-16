class Menu {
  def main(args: Array[String]): Unit = {
    var choice = 0

    while (choice != 7) {
      displayMenu()
      choice = scala.io.StdIn.readInt()

      //choice match {
      // case 1 => displayResults(DataAnalyse().getCurrentPrices())
      // case 2 => displayResults(getMinMaxPrices)
      // case 3 => displayResults(getMedianPrices)
      // case 4 => println(s"The rising food is: ${getRisingFood}")
      // case 5 => compareAverageValues(getUserInput("Enter food 1: "), getUserInput("Enter food 2: "))
      // case 6 => println(s"The basket total is: ${calculateBasketTotal(getBasketFromUser)}")
      // case 7 => println("Exiting...")
      // case _ => println("Invalid choice. Please enter a number between 1 and 7.")
      //}
    }
  }

  def displayMenu(): Unit = {
    println("1. Get current price for each food")
    println("2. Get highest and lowest prices for each food")
    println("3. Get median price for each food")
    println("4. Get the symbol for the food which has risen most over the last 6 months")
    println("5. Compare average values of two foods")
    println("6. Calculate basket total")
    println("7. Quit")
    print("Enter your choice: ")
  }

  def getUserInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine()
  }

  def getBasketFromUser: Map[String, Double] = {
    // Implementation logic for getting a basket from the user
    // ...

    Map("RICE" -> 2.5, "BEEF" -> 0.5, "APPLE" -> 1.0)
  }

  def displayResults(results: Any): Unit = {
    
  }
}
