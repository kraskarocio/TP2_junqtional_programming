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
 * @param path A string representing a path in the JSON structure (used for operations like "get" and "delete").
 * @param value A string representing the value or transformation to apply in the operation.
 * @param json The input JSON-like structure on which the operation is performed.
 * @return The result of the operation on the JSON-like structure.
 * @throws IllegalArgumentException If the function name or value is unknown or invalid.
 */

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
      delete(value, json)
    case "exists_key" => existsKey(json, value)
    case "merge" =>
      val jsonValue = jsonParser(value)
      merge(json, jsonValue)
    
    case _ =>
      throw new IllegalArgumentException(s"Unknown function: $functionName")
  }
}