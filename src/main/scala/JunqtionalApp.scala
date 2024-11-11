import scala.io.Source
import parser.jsonParser
import paths.getPathResult
import functions.handler
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
    res = mapToJsonString(handler(option, null, func, mapJson))
    println("--- PATH(res) ---")
    println(res)

  } catch {
    case e: Exception => println(s"Error: ${e.getMessage}")
  }
}

