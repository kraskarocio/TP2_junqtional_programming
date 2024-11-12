package functions
import functions.*
import parser.*
import paths.{getPathResult, tokenize}


/**
 * @brief Handles different JSON operations based on the provided function name and value.
 * This function matches the provided `functionName` to determine which operation to perform on the JSON-like structure.
 * The operation can involve transforming elements, retrieving values, checking for keys, deleting items, or merging JSON objects.
 * The specific behavior is determined by the value associated with the function name.
 * @param functionName The name of the function to execute, determining which operation to apply to the JSON structure.
 * @param args1 A string representing a path in the JSON structure (used for operations like "get" and "delete").
 * @param args2 A string representing the value or transformation to apply in the operation.
 * @param json The input JSON-like structure on which the operation is performed.
 * @return The result of the operation on the JSON-like structure.
 * @throws IllegalArgumentException If the function name or value is unknown or invalid.
 */
def handler(functionName: String, args1: String, args2: String, args3: String, json: Any): Any = {
  functionName match {
    case "map_rec" =>
      args1 match {
        case "toUpper" => mapRec(json, toUpperCaseTransformer)
        case "reverse" => mapRec(json, reverseString)
        case "sum1" => mapRec(json, sum1)
        case "negate" => mapRec(json, negateBooleans)
        case "mul2" => mapRec(json, mul2)
        case _ => throw new IllegalArgumentException(s"Unknown transformer: $args1")
      }
    case "depth" => depth(json, args1.toInt)
    case "get" => getPathResult(args1, json)
    case "delete" =>
      delete(args1, json)
    case "exists_key" => existsKey(json, args1)
    case "merge" =>
      val jsonValue = jsonParser(args1)
      println(jsonValue)
      merge(json, jsonValue)
    case "add_item" =>
      val token = tokenize(args1)
      val args2Json = jsonParser(args2)
      addItem(token, args2Json, json)
    case "map" =>
      args1 match {
        case "toUpper" => map_json(json, toUpperCaseTransformer)
        case "reverse" => map_json(json, reverseString)
        case "sum1" => map_json(json, sum1)
        case "negate" => map_json(json, negateBooleans)
        case "mul2" => map_json(json, mul2)
        case _ => throw new IllegalArgumentException(s"Unknown condi: $args1")
      }
    case "add_key" =>
      val tokens = tokenize(args1)
      addKey(tokens, args2, args3, json)
    case _ =>
      throw new IllegalArgumentException(s"Unknown function: $functionName")
  }
}