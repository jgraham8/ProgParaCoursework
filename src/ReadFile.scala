import scala.io.Source
class ReadFile {
  def main(filename: String): Map[String, List[Int]] = {
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
}
