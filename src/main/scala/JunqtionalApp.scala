import scala.io.Source
import parser.JsonParser.*
import mapToJson.mapToJsonString

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      if (args.isEmpty) {
        throw new IllegalArgumentException("No file path provided.")
      }

      val filePath = args(0)
      val input = Source.fromFile(filePath).getLines().mkString
      val mapJson = jsonParser(input)
      println(mapJson)
      val string = mapToJsonString(mapJson)
      println(string)
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
