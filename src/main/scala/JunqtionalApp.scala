import scala.io.Source
import scala.util.{Try, Failure, Success}
import parser._

object JunqtionalApp {

  def readFile(): String = {
    val input = Try(Source.stdin.getLines().mkString("\n")).getOrElse("")
    if (input.isEmpty) {
      throw new Exception("ERR: No path found")
    }
    input
  }
  def main(args: Array[String]): Unit = {
    try {
      val output = """{"hola": 1}"""
      val jsonMap = scanner(output)

      // Imprime el mapa resultante
      println(jsonMap)
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }
}
