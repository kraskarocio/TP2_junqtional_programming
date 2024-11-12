package parser
import parser.Token.*

/**
 * @brief Processes a list of tokens into a JSON-like structure (Map or List).
 * This function recursively processes a list of tokens and constructs a corresponding 
 * JSON structure. It distinguishes between objects (Map) and arrays (List) and handles 
 * them accordingly.
 * @param tokens The list of tokens to process.
 * @return A JSON-like structure representing the parsed tokens.
 */
def processTokens(tokens: List[(Token, Any)]): Any = {

  /**
   * @brief Helper function to process object tokens into a Map.
   * This function processes a list of tokens that represent a JSON object and 
   * recursively adds key-value pairs to an accumulator Map.
   * @param tokens The list of tokens to process.
   * @param acc The accumulator Map that stores key-value pairs.
   * @return A Map with the parsed key-value pairs and remaining tokens.
   */
  def helperMap(tokens: List[(Token, Any)], acc: Map[String, Any]): Map[String, Any] = {
    tokens match {
      case (L_BRACE, _) :: rest =>
        val (newAcc, remainingTokens) = processObject(rest, Map())
        helperMap(remainingTokens, acc ++ newAcc)

      case (STRING, key: String) :: (COLON, _) :: rest =>
        val (value, nextTokens) = extractValue(rest)
        helperMap(nextTokens, acc + (key -> value))

      case _ =>
        acc
    }
  }

  /**
   * @brief Helper function to process array tokens into a List.
   * This function processes a list of tokens that represent a JSON array and 
   * recursively adds values to an accumulator List.
   * @param tokens The list of tokens to process.
   * @param acc The accumulator List that stores array elements.
   * @return A List with the parsed elements and remaining tokens.
   */
  def helperList(tokens: List[(Token, Any)], acc: List[Any]): List[Any] = {
    tokens match {
      case (L_BRACKET, _) :: rest =>
        val (newAcc, remainingTokens) = processArray(rest)
        helperList(remainingTokens, acc ++ newAcc)

      case _ =>
        acc
    }
  }

  tokens match {
    case (L_BRACE, _) :: _ => helperMap(tokens, Map())
    case (L_BRACKET, _) :: _ => helperList(tokens, List())
    case _ => throw new IllegalArgumentException("Expected JSON object or array")
  }
}

/**
 * @brief Extracts a value from the token list based on its type.
 * This function identifies the type of the token (e.g., Number, String, Boolean) 
 * and extracts the corresponding value, returning it along with the remaining tokens.
 * @param tokens The list of tokens to extract a value from.
 * @return A tuple containing the extracted value and the remaining tokens.
 */
def extractValue(tokens: List[(Token, Any)]): (Any, List[(Token, Any)]) = {
  tokens match {
    case (NUMBER, value) :: rest => (value, rest)
    case (STRING, value) :: rest => (value, rest)
    case (BOOLEAN, value) :: rest => (value, rest)
    case (NULL, value) :: rest => (value, rest)
    case (L_BRACE, _) :: rest =>
      val (innerMap, nextTokens) = processObject(rest, Map())
      (innerMap, nextTokens)
    case (L_BRACKET, _) :: rest =>
      val (list, nextTokens) = processArray(rest)
      (list, nextTokens)
    case _ => throw new Exception("Unexpected token while extracting value.")
  }
}

/**
 * @brief Processes a list of tokens into a JSON object (Map).
 * This function processes tokens that represent a JSON object and recursively 
 * builds the Map of key-value pairs, handling various token scenarios such as 
 * commas, braces, and nested objects.
 * @param tokens The list of tokens to process.
 * @param acc The accumulator Map for storing key-value pairs.
 * @return A tuple containing the parsed Map and the remaining tokens.
 */
def processObject(tokens: List[(Token, Any)], acc: Map[String, Any]): (Map[String, Any], List[(Token, Any)]) = {
  tokens match {
    case (R_BRACE, _) :: rest => (acc, rest)
    case (STRING, key: String) :: (COLON, _) :: valueTokens =>
      val (value, nextTokens) = extractValue(valueTokens)
      nextTokens match {
        case (COMMA, _) :: remainingTokens =>
          processObject(remainingTokens, acc + (key -> value))
        case (R_BRACE, _) :: rest => 
          (acc + (key -> value), rest)
        case _ =>
          throw new Exception("ERR: Expected comma")
      }
    case _ =>
      throw new Exception("ERR: Invalid JSON object format")
  }
}

/**
 * @brief Processes a list of tokens into a JSON array (List).
 * This function processes tokens that represent a JSON array, recursively 
 * adding elements to a List until the closing bracket is encountered.
 * @param tokens The list of tokens to process.
 * @return A tuple containing the parsed List and the remaining tokens.
 */
def processArray(tokens: List[(Token, Any)]): (List[Any], List[(Token, Any)]) = {
  tokens match {
    case (R_BRACKET, _) :: rest => (List(), rest)
    case _ =>
      val (value, nextTokens) = extractValue(tokens)
      nextTokens match {
        case (COMMA, _) :: remainingTokens =>
          val (values, finalTokens) = processArray(remainingTokens)
          (value :: values, finalTokens)
        case _ =>
          val (values, finalTokens) = processArray(nextTokens)
          (value :: values, finalTokens)
      }
  }
}