import scala.io.Source
import parser.JsonParser.*
import paths.getPathResult
import functions.handler
import mapToJson.mapToJsonString
object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      val option = "exists_key"
      val func = "v"
      val pathFile = "resources/EX1.json"
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)
      var res: Any = null
      if(option.startsWith(".")){
        res = getPathResult(option, mapJson)
      }
      res = handler(option, null, func, mapJson)
      val resStr = mapToJsonString(res)
      println("--- (res) ---")
      println(resStr)
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
