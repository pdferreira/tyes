package tyes.compiler.ir

enum CodeGenNode:
  case If(cond: CodeGenNode, thenBranch: CodeGenNode, elseBranch: CodeGenNode)
  case For(cursors: Seq[CodeGenForCursor], body: CodeGenNode)
  case Throw(excClass: String, excMessage: CodeGenNode)
  case Text(str: String)
  case FormattedText(formattingStr: (String | CodeGenNode)*)
  case Integer(n: Int)
  case Unit
  case Var(str: String)
  case Field(obj: CodeGenNode, field: String)
  case Not(exp: CodeGenNode)
  case Equals(left: CodeGenNode, right: CodeGenNode)
  case NotEquals(left: CodeGenNode, right: CodeGenNode)
  case Apply(fun: CodeGenNode, args: CodeGenNode*)
  case Let(varName: String, varExp: CodeGenNode, bodyExp: CodeGenNode)
  case Match(matchedExp: CodeGenNode, branches: Seq[(CodeGenNode, CodeGenNode)])
  case Return(exp: CodeGenNode)

enum CodeGenForCursor:
  case Iterate(name: String, collection: CodeGenNode)
  case Let(name: String, exp: CodeGenNode)
  case Filter(exp: CodeGenNode)

object CodeGenNodeOperations extends CodeOperations[CodeGenNode]:

  def negate(code: CodeGenNode): CodeGenNode = code match {
    case CodeGenNode.Equals(l, r) => CodeGenNode.NotEquals(l, r)
    case CodeGenNode.NotEquals(l, r) => CodeGenNode.Equals(l, r)
    case _ => CodeGenNode.Not(code)
  }

def compile(cgNode: CodeGenNode, indentLevel: Int = 0): String = 
  val indent = "  ".repeat(indentLevel)
  cgNode match {
    case CodeGenNode.If(cond, thenBranch, elseBranch) =>
      val condStr = compile(cond)
      val thenStr = compile(thenBranch, indentLevel + 1)
      val elseStr = compile(elseBranch, indentLevel + 1)
      s"${indent}if $condStr then\r\n$thenStr\r\n${indent}else\r\n$elseStr"
    case CodeGenNode.For(cursors, body) =>
      val cursorsStr = cursors
        .map(compile)
        .map(cStr => s"${indent}  $cStr\r\n")
        .mkString
      var bodyStr = compile(body, indentLevel + 1)
      s"${indent}for\r\n$cursorsStr${indent}yield\r\n$bodyStr"
    case CodeGenNode.Throw(excClass, excMessage) =>
      val msgStr = compile(excMessage)
      s"${indent}throw new $excClass($msgStr)"
    case CodeGenNode.Text(str) => s"${indent}\"$str\""
    case CodeGenNode.FormattedText(formattingStrs*) =>
      formattingStrs
        .map({
          case s: String => s
          case node: CodeGenNode => 
            val nodeStr = compile(node)
            s"$${$nodeStr}"
        })
        .mkString(s"${indent}s\"", "", "\"")
    case CodeGenNode.Integer(n) => s"${indent}$n"
    case CodeGenNode.Unit => s"${indent}()"
    case CodeGenNode.Not(exp) => s"${indent}!${compile(exp)}"
    case CodeGenNode.Equals(l, r) => s"${indent}${compile(l)} == ${compile(r)}"
    case CodeGenNode.NotEquals(l, r) => s"${indent}${compile(l)} != ${compile(r)}"
    case CodeGenNode.Apply(fun, args*) => 
      val funStr = compile(fun, indentLevel)
      val argsStr = args.map(compile(_)).mkString(", ")
      s"${funStr}(${argsStr})"
    case CodeGenNode.Let(name, exp, body) =>
      val expStr = compile(exp)
      val bodyStr = compile(body, indentLevel)
      s"${indent}val $name = $expStr\r\n$bodyStr"
    case CodeGenNode.Var(name) => s"${indent}$name"
    case CodeGenNode.Field(obj, field) =>
      val objStr = compile(obj, indentLevel)
      s"$objStr.$field"
    case CodeGenNode.Match(matchedExp, branches) =>
      val matchedStr = compile(matchedExp, indentLevel)
      val matchesStr = 
        (
          for case (patExp, thenExp) <- branches
          yield
            val patStr = compile(patExp)
            val thenStr = compile(thenExp)
            s"${indent}  case $patStr => $thenStr"
        )
        .mkString("{\r\n", "\r\n", s"\r\n$indent}")

      s"$matchedStr match $matchesStr"
    case CodeGenNode.Return(exp) => s"${indent}return ${compile(exp)}"
  }

def compile(cgCursor: CodeGenForCursor): String = cgCursor match {
  case CodeGenForCursor.Filter(exp) => s"if ${compile(exp)}"
  case CodeGenForCursor.Iterate(name, collection) => s"$name <- ${compile(collection)}"
  case CodeGenForCursor.Let(name, exp) => s"$name = ${compile(exp)}"
}