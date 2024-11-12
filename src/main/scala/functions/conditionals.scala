package functions

/**
 * @brief Checks if a given number is greater than 10.
 * This function evaluates whether the provided value is an integer greater than 10. 
 * If the value is not an integer, it returns false.
 * @param num The input value, expected to be an integer.
 * @return True if the value is an integer greater than 10, otherwise false.
 */
val greaterThan10: Any => Boolean = {
  case num: Int => num > 10
  case _ => false
}

/**
 * @brief Checks if a given value is true.
 * This function evaluates whether the provided value is a boolean `true`. 
 * If the value is not a boolean, it returns false.
 * @param b The input value, expected to be a boolean.
 * @return True if the value is boolean `true`, otherwise false.
 */
val isTrue: Any => Boolean = {
  case b: Boolean => b
  case _ => false
}