package functions
import functions.*
import parser.JsonParser.jsonParser
import paths.{getPathResult, tokenize}

def handler(functionName: String, path: String, value: String, json: Any): Any = {
  functionName match {
    case "map_rec" =>
      value match {
        case "toUpper" => mapRec(json, toUpperCaseTransformer)
        case "reverse" => mapRec(json, reverseString)
        case "sum1" => mapRec(json, sum1)
        case "negate" => mapRec(json, negateBooleans)
        case "mul2" => mapRec(json, mul2)
        case _ => throw new IllegalArgumentException(s"Unknown transformer: $value")
      }
    case "depth" => depth(json, value.toInt)
    case "get" => getPathResult(value, json)
    case "delete" =>
      val tokens = tokenize(value)
      delete(tokens, json)
    case "exists_key" => exists_key(json, value)
    case "merge" =>
      val jsonValue = jsonParser(value)
      merge(json, jsonValue)
    
    case _ =>
      throw new IllegalArgumentException(s"Unknown function: $functionName")
  }
}