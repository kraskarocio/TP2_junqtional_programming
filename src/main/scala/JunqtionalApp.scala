import scala.io.Source
import parser.jsonParser
import paths.getPathResult
import functions.{existsKey, get,  handler, delete}
import mapToJson.mapToJsonString



@main def main(command: String = null, arg1: String = null, arg2: String = null, arg3: String = null, output: String = null): Unit = {
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
      println("\n--- TESTING EXISTS-KEY ---")

      println(existsKey(mapJson, ".[0]"))
      println(existsKey(mapJson, ".[0].chau"))
      println(existsKey(mapJson, ".[0].c"))
      println(existsKey(mapJson, ".chau"))
      println(existsKey(mapJson, ".NonExists"))

      println("\n--- TESTING GET ---")

      println(get(mapJson, ".[0].chau.c"))
      println(get(mapJson, ".[1]"))
      println(get(mapJson, ".[3]"))
      println(get(mapJson, ".[0].chau"))

      println("--- TESTING DELETE ---")

      println(delete(".[0].chau.c", mapJson))
      println(delete(".[1]", mapJson))
      println(delete(".[3]", mapJson))
      println(delete(".[0].chau", mapJson))

      println(delete(".[0].NonExists", mapJson))
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
    
    res = mapToJsonString(handler(option, null, func, mapJson))
    println("--- PATH(res) ---")
    println(res)

  } catch {
    case e: Exception => println(s"Error: ${e.getMessage}")
  }
}

