import scala.io.Source
import parser.JsonParser.*
import paths.getPathResult

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      val option = ".a.b.c.d.[1]"
      val pathFile = "resources/EX1.json"
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
