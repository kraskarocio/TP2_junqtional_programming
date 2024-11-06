import io.circe._
import io.circe.parser._
import scala.io.Source 

object JunqtionalApp {
  def printJson(json: Json): Unit = {
    println(json)
  }
  
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("ERR: No arguments provided")
      return
    }
    val func = args(0) // function to run
    val path = args(1) // path to JSON file
          // read JsonFile
    val jsonString = try {
      Source.fromFile(path).mkString
    } catch {
      case e: Exception => 
        println(s"Error reading the file: ${e.getMessage}")
        return
    } 
    val json: Either[ParsingFailure, Json] = parse(jsonString)
    json match {
      case Right(parsedJson) =>
        // if the JSON is valid, choose which function to run based on the first argument
        func match {
          case "print" => printJson(parsedJson)
          // Add more functions here ...
          case _ => println(s"ERR: Unknown function '$func'")
        }
      case Left(error) =>
        println(s"Error parsing the JSON: $error")
    }
  }
}
