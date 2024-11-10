package functions

val toUpperCaseTransformer: Any => Any = {
  case str: String => str.toUpperCase
  case other => other
}

val reverseString: Any => Any = {
  case str: String => str.reverse
  case other => other
}

val mul2: Any => Any = {
  case num: Int => num * 2
  case other => other
}

val sum1: Any => Any = {
  case num: Int => num + 1
  case other => other
}


val negateBooleans: Any => Any = {
  case b: Boolean => !b
  case other => other
}
