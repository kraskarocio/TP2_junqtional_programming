import scala.io.Source
import parser.JsonParser.*
import paths.getPathResult
import functions.{existsKey, get,  handler}
import mapToJson.mapToJsonString
object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {

      val pathFile = "resources/EX1.json"
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)

      println(mapJson)

      println("\n--- GET ---")
      println(get(mapJson, ".[0].chau.c"))
      println(get(mapJson, ".[1]"))
      println(get(mapJson, ".[3]"))
      println(get(mapJson, ".[0].chau"))

      println("\n--- EXISTS-KEY ---")
      println(existsKey(mapJson, ".[0]"))
      println(existsKey(mapJson, ".NonExists"))
      
      val option = "merge"
      val func = """[{"d":1},1]"""
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
