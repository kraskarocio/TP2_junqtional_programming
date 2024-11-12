package parser
import Token.*

/**
* Scanner function that takes a string input and returns a list of tokens and their values.
* @param input The input string to be scanned.
* @return A list of tokens and their values.
*/
def scanner(input: String): List[(Token, Any)] = {
  def returnCurrent(input: String, current: Int): Char = {
    input.charAt(current)
  }
  def isEOF(input: String, current: Int): Boolean = {
    input.length <= current
  }
  def returnNext(input: String, current: Int): Option[Char] = {
    if (isEOF(input, current)) {
      None
    } else {
      Some(input.charAt(current + 1))
    }
  }

/**
* @brief Helper function to get a string from the input.
* @param input The input string to be scanned.
* @param current The current index in the input string.
* @return A tuple containing the string and the next index.
*/
  def getString(input: String, current: Int): (String, Int) = {
    def helper(index: Int, acc: String): (String, Int) = {
      if (index >= input.length || input.charAt(index) == '"') {
        (acc, index + 1)
      } else {
        helper(index + 1, acc + input.charAt(index))
      }
    }

    helper(current + 1, "")
  }

/**
* @brief Helper function to get a digit from the input.
* @param input The input string to be scanned.
* @param current The current index in the input string.
* @return A tuple containing the digit and the next index.
*/
  def getDigit(input: String, current: Int): (Any, Int) = {
    def helper(index: Int, acc: String): (String, Int) = {
      if (index >= input.length || input.charAt(index).isWhitespace || "[},]".contains(input.charAt(index))) {
        (acc, index)
      } else {
        helper(index + 1, acc + input.charAt(index))
      }
    }
    val (str, ind) = helper(current, "")
    if (str.contains(".")) {
      (str.toFloat, ind)
    } else {
      (str.toInt, ind)
    }
  }

/**
* @brief Helper function to get a keyword from the input.
* @param input The input string to be scanned.
* @param current The current index in the input string.
* @return A tuple containing the token and the next index.
*/
  def getKeyword(input: String, current: Int): Option[(Token, Int)] = {
    val keywords = Map("true" -> BOOLEAN, "false" -> BOOLEAN, "null" -> NULL)

    def helper(index: Int, acc: String): Option[(Token, Int)] = {
      if (index >= input.length || input.charAt(index).isWhitespace || "[},]".contains(input.charAt(index))) {
        keywords.get(acc).map(token => (token, index))
      } else {
        helper(index + 1, acc + input.charAt(index))
      }
    }

    helper(current, "")
  }

/**
* @brief Helper function to scan the input string.
* @param input The input string to be scanned.
* @param current The current index in the input string.
* @return A list of tokens and their values.
*/
  def scan(input: String, current: Int = 0): List[(Token, Any)] = {
    if (current >= input.length) {
      List((EOF, "")) // End of file (EOF) token
    } else {
      val currentChar = input.charAt(current)
      val (token, consumed, nextIndex) = currentChar match {
        case '{' => (L_BRACE, "{", current + 1)
        case '}' => (R_BRACE, "}", current + 1)
        case '[' => (L_BRACKET, "[", current + 1)
        case ']' => (R_BRACKET, "]", current + 1)
        case ',' => (COMMA, ",", current + 1)
        case ':' => (COLON, ":", current + 1)
        case '-' =>
          val (number, nextIndex) = getDigit(input, current)
          (NUMBER, number, nextIndex)
        case c if c.isDigit =>
          val (number, nextIndex) = getDigit(input, current)
          (NUMBER, number, nextIndex)
        case '"' =>
          val (str, nextIndex) = getString(input, current)
          (STRING, str, nextIndex)
        case _ if currentChar.isWhitespace =>
          return scan(input, current + 1)
        case _ =>
          val token = getKeyword(input, current)
          token match
            case Some((BOOLEAN, nextIndex)) if input.startsWith("true", current) =>
              (BOOLEAN, true, nextIndex)

            case Some((BOOLEAN, nextIndex)) if input.startsWith("false", current) =>
              (BOOLEAN, false, nextIndex)

            case Some((NULL, nextIndex)) if input.startsWith("null", current) =>
              (NULL, null, nextIndex)

            case Some((unknownToken, nextIndex)) =>
              throw new Exception(s"ERR: Unexpected token ${unknownToken}")

            case None =>
              throw new Exception("ERR: Invalid JSON format")
      }

      (token, consumed) :: scan(input, nextIndex)
    }
  }

  scan(input, 0)
}
