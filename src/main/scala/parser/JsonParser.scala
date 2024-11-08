package parser

object JsonParser {
  def jsonParser(input: String): Map[String, Any] = {
    val tokens = scanner(input)
    val map = processTokens(tokens)
    map
  }
}
