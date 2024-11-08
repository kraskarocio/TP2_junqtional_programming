import scala.io.Source
import parser.JsonParser.*

object JunqtionalApp {
  def main(args: Array[String]): Unit = {
    try {
      val input = if (args.nonEmpty) args.mkString else Source.stdin.getLines().mkString
      val jsonMap = jsonParser(input)
      println(jsonMap)
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
