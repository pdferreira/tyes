package tyes.compiler.ir

trait CodeOperations[TCode]:
  def negate(code: TCode): TCode

object StringCodeOperations extends CodeOperations[String]:
  def negate(code: String): String =
    if code.contains(' ') 
    then s"!($code)" 
    else s"!$code"
