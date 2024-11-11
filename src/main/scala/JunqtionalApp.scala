import scala.io.Source
import parser.JsonParser.*
import paths.getPathResult
import functions.{existsKey, get,  handler}
import mapToJson.mapToJsonString
object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      //val option = "existsKey"
      //val func = "v"
      val pathFile = "resources/EX1.json"
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)
      println("\n--- TESTING EXISTS-KEY ---")

      // Pruebas de existsKey
      println(s"existsKey(mapJson, \".[0]\"): ${existsKey(mapJson, ".[0]")}") // true
      println(s"existsKey(mapJson, \".[0].chau\"): ${existsKey(mapJson, ".[0].chau")}") // true
      println(s"existsKey(mapJson, \".[0].c\"): ${existsKey(mapJson, ".[0].c")}") // false
      println(s"existsKey(mapJson, \".chau\"): ${existsKey(mapJson, ".chau")}") // false
      println(s"existsKey(mapJson, \".NonExists\"): ${existsKey(mapJson, ".NonExists")}") // false

      println("\n--- TESTING GET ---")

      // Pruebas de get
      println(s"get(mapJson, \".[0].chau.c\"): ${get(mapJson, ".[0].chau.c")}") // true
      println(s"get(mapJson, \".[1]\"): ${get(mapJson, ".[1]")}") // 9
      println(s"get(mapJson, \".[3]\"): ${get(mapJson, ".[3]")}") // "hola"
      println(s"get(mapJson, \".[0].chau\"): ${get(mapJson, ".[0].chau")}") // {"c": true}
      /*
      var res: Any = null
      if(option.startsWith(".")){
        res = getPathResult(option, mapJson)
      }
      res = handler(option, null, func, mapJson)
      val resStr = mapToJsonString(res)
      println("--- (res) ---")
      println(resStr)
      */
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
