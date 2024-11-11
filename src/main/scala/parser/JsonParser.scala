package parser

def jsonParser(input: String):  Any = {
  val tokens = scanner(input)
  println(tokens)
  val map = processTokens(tokens)
  map
}
