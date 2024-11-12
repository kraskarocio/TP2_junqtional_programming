package parser

/**
 * @brief Parses a JSON string into a JSON-like structure.
 * This function takes a JSON-formatted string as input, scans it to extract tokens, 
 * and processes those tokens to build a corresponding JSON structure. 
 * The structure can be a Map, List, or other types based on the JSON content.
 * @param input The input JSON string to be parsed.
 * @return A JSON-like structure (Map, List, etc.) representing the parsed content of the input string.
 */
def jsonParser(input: String):  Any = {
  val tokens = scanner(input)
  println(tokens)
  val map = processTokens(tokens)
  map
}