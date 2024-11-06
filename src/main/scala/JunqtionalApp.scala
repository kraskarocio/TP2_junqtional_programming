import io.circe._
import io.circe.parser._

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    val jsonString = scala.io.Source.fromFile("resources/EX1.json").mkString
    val json: Either[ParsingFailure, Json] = parse(jsonString)
    print(json)
  }
}
