import scala.io.Source
import parser.JsonParser.*
import mapToJson.mapToJsonString
import paths.getPathResult

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      val option = args(0)
      val pathFile = args(1)
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)
      var pathRes: Any = null
      if(option.startsWith(".")){
        pathRes = getPathResult(option, mapJson)
      }
      println("--- PATH(res) ---")
      println(pathRes)

    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
