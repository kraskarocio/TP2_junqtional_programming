import scala.io.Source
import parser.JsonParser.*
import paths.getPathResult
import functions.handler
import mapToJson.mapToJsonString
object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      val option = "merge"
      val func = """[{"d":1},1]"""
      val pathFile = "resources/EX1.json"
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)
      var res: Any = null
      if(option.startsWith(".")){
        res = getPathResult(option, mapJson)
      }
      res = mapToJsonString(handler(option, null, func, mapJson))
      println("--- PATH(res) ---")
      println(res)

    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
