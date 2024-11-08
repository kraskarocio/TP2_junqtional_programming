package parser
import parser.Token.*

def processTokens(tokens: List[(Token, Any)]): Any = {
  // Función auxiliar para procesar objetos en el JSON
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

  // Función auxiliar para procesar listas en el JSON
  def helperList(tokens: List[(Token, Any)], acc: List[Any]): List[Any] = {
    tokens match {
      case (L_BRACKET, _) :: rest =>
        val (newAcc, remainingTokens) = processArray(rest)
        helperList(remainingTokens, acc ++ newAcc)

      case _ =>
        acc
    }
  }

  // Determina si es un objeto (Map) o una lista (List) según el primer token
  tokens match {
    case (L_BRACE, _) :: _ => helperMap(tokens, Map())
    case (L_BRACKET, _) :: _ => helperList(tokens, List())
    case _ => throw new IllegalArgumentException("Expected JSON object or array")
  }
}

def extractValue(tokens: List[(Token, Any)]): (Any, List[(Token, Any)]) = {
  tokens match {
    case (NUMBER, value) :: rest => (value, rest)
    case (STRING, value) :: rest => (value, rest)
    case (BOOLEAN, value) :: rest => (value, rest) // Para true o false
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

def processObject(tokens: List[(Token, Any)], acc: Map[String, Any]): (Map[String, Any], List[(Token, Any)]) = {
  tokens match {
    case (R_BRACE, _) :: rest => (acc, rest) // fin del objeto
    case (STRING, key: String) :: (COLON, _) :: valueTokens =>
      val (value, nextTokens) = extractValue(valueTokens)
      nextTokens match {
        case (COMMA, _) :: remainingTokens => // Salta la coma y continúa
          processObject(remainingTokens, acc + (key -> value))
        case _ =>
          processObject(nextTokens, acc + (key -> value))
      }
    case _ =>
      throw new Exception("ERR: Invalid JSON object format")
  }
}

def processArray(tokens: List[(Token, Any)]): (List[Any], List[(Token, Any)]) = {
  tokens match {
    case (R_BRACKET, _) :: rest => (List(), rest) // fin de la lista
    case _ =>
      val (value, nextTokens) = extractValue(tokens)
      nextTokens match {
        case (COMMA, _) :: remainingTokens => // Salta la coma y continúa
          val (values, finalTokens) = processArray(remainingTokens)
          (value :: values, finalTokens)
        case _ =>
          val (values, finalTokens) = processArray(nextTokens)
          (value :: values, finalTokens)
      }
  }
}
