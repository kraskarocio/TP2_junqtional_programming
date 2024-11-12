package functions

val greaterThan10: Any => Boolean = {
  case num: Int => num > 10
  case _ => false
}

val isTrue: Any => Boolean = {
  case b: Boolean => b
  case _ => false
}