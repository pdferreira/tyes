package tyes.compiler.ir

object StringCodeOperations extends CodeOperations[String]:
  def negate(code: String): String =
    if code.contains(' ') 
    then s"!($code)" 
    else s"!$code"
