
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import scala.io.Source 

object JunqtionalApp {
  def printJson(json: Json): Unit = {
    println(json)
  }

  def get(json: Json, path: String): Json = {
    val keys = path.split("\\.").toList
    def getSearch(currentJson: Json, keys: List[String]): Json = keys match {
      case Nil => currentJson
      case head :: tail =>
        currentJson.asObject match {
          case Some(obj) =>
            obj.toMap.get(head) match {
              case Some(nextJson) => getSearch(nextJson, tail)
              case None => Json.Null
            }
          case _ => Json.Null
        }
    }
    getSearch(json, keys)
  }
  def main(args: Array[String]): Unit = {
    val func = "print" // No usamos argumentos de línea de comando
    val path = "resources/EX1.json" // El archivo JSON está en resource
    /*
    if (args.isEmpty) {
      println("ERR: No arguments provided")
      return
    }
    val func = args(0) // function to run
    val path = args(1) // path to JSON file
          // read JsonFile

    */

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
        println(get(parsedJson, "usuarioTriple.nombre"))
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
