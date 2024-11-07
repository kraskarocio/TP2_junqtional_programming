package parser
import Token.*

import scala.annotation.tailrec

def scanner(input: String): List[(Token, String)] = {
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

  def getDigit(input: String, current: Int): (String, Int) = {
    def helper(index: Int, acc: String): (String, Int) = {
      if (index >= input.length || !input.charAt(index).isDigit) {
        (acc, index)
      } else {
        helper(index + 1, acc + input.charAt(index))
      }
    }

    helper(current, "")
  }

  def getKeyword(input: String, current: Int): Option[(Token, String, Int)] = {
    val keywords = Map("true" -> BOOLEAN, "false" -> BOOLEAN, "null" -> NULL)

    def helper(index: Int, acc: String): Option[(Token, String, Int)] = {
      if (index >= input.length || input.charAt(index).isWhitespace) {
        keywords.get(acc).map(token => (token, acc, index))
      } else {
        helper(index + 1, acc + input.charAt(index))
      }
    }

    helper(current, "")
  }

  def scan(input: String, current: Int = 0): List[(Token, String)] = {
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
          return scan(input, current + 1) // Skip whitespace and continue scanning
        case _ =>
          getKeyword(input, current) match {
            case Some((token, value, nextIndex)) => (token, value, nextIndex)
            case None => throw new Exception("ERR: Invalid JSON format")
          }
      }

      // Recursively scan the rest of the input
      (token, consumed) :: scan(input, nextIndex)
    }
  }

  scan(input, 0)
}
