package paths

import scala.util.matching.Regex

val dotRegex: Regex = "^\\.".r
val pipelineRegex: Regex = "^\\|".r
val lBraceRegex: Regex = "^\\[".r
val rBraceRegex: Regex = "^\\]".r
val numRegex: Regex = "^\\d+".r
val strRegex: Regex = "^[a-zA-Z_][a-zA-Z0-9_]*".r

def tokenize(path: String): List[(PathToken, String)] = {
    def tokenizeRecursive(input: String, current: Int): List[(PathToken, String)] = {
      if (current >= input.length) return Nil
      val remaining = input.substring(current)
      if (dotRegex.findPrefixOf(remaining).isDefined) {
        (PathToken.DOT, "") :: tokenizeRecursive(input, current + 1)

      } else if (pipelineRegex.findPrefixOf(remaining).isDefined) {
        (PathToken.PIPELINE, "") :: tokenizeRecursive(input, current + 1)

      } else if (lBraceRegex.findPrefixOf(remaining).isDefined) {
        (PathToken.L_BRACE, "") :: tokenizeRecursive(input, current + 1)

      } else if (rBraceRegex.findPrefixOf(remaining).isDefined) {
        (PathToken.R_BRACE, "") :: tokenizeRecursive(input, current + 1)

      } else if (numRegex.findPrefixOf(remaining).isDefined) {
        val numMatch = numRegex.findPrefixOf(remaining).get
        (PathToken.NUM, numMatch) :: tokenizeRecursive(input, current + numMatch.length)

      } else if (strRegex.findPrefixOf(remaining).isDefined) {
        val strMatch = strRegex.findPrefixOf(remaining).get
        (PathToken.STR, strMatch) :: tokenizeRecursive(input, current + strMatch.length)

      } else {
        throw new IllegalArgumentException(s"Invalid path format: $remaining")
      }
    }
  tokenizeRecursive(path, 0)
}

def extractTokens(tokensWithValues: List[(PathToken, String)]): List[PathToken] = {
  tokensWithValues.map { case (t, _) => t }
}