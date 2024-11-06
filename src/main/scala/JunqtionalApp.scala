import io.circe._
import io.circe.parser._
import scala.io.Source 

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("ERR: No arguments provided")
      return
    }
    val path = args(0)
    try {
      // read JsonFile
      val jsonString = Source.fromFile(path).mkString

      // parse JSON using Circe
      val json: Either[ParsingFailure, Json] = parse(jsonString)

      json match {
        case Right(parsedJson) =>
          // if JSON was parsed correctly, print it
          println(s" $parsedJson")
        case Left(error) =>
          // if there was an error parsing the JSON, print the error
          println(s"PARSING ERR: $error")
      }
    } catch {
      case e: Exception =>
        // if there was an error reading the file, print the error
        println(s"READING ERR: ${e.getMessage}")
    }
  }
}
