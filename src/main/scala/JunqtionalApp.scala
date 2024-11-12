import scala.io.Source
import parser.jsonParser
import paths.getPathResult
import functions.{existsKey, get,  handler, delete}
import mapToJson.mapToJsonString
import scala.io.Source

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    val command = if (args.length > 0) args(0) else null
    val arg1 = if (args.length > 1) args(1) else null
    val arg2 = if (args.length > 2) args(2) else null
    val arg3 = if (args.length > 3) args(3) else null
    val output = if (args.length > 4) args(4) else null

    try {
      val option = "."
      val input = Source.stdin.getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)
      val res = getPathResult(option, mapJson)
      println("--- PATH(res) ---")
      println(res)
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}