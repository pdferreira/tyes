package tyes.compiler.ir

class ScalaIRCodeGenerator extends IRCodeGenerator:
  
  private def indentIfMultiline(codeStr: String, indentLevel: Int): String =
    val lines = codeStr.linesWithSeparators.toSeq
    if lines.length <= 1 then
      codeStr
    else
      val indent = "  ".repeat(indentLevel)
      lines.map(_.prependedAll(indent)).mkString("\r\n", "", "\r\n")

  def generate(cgNode: CodeGenNode): String = generate(cgNode, indentLevel = 0)

  def generate(cgNode: CodeGenNode, indentLevel: Int): String = 
    val indent = "  ".repeat(indentLevel)
    cgNode match {
      case CodeGenNode.If(cond, thenBranch, elseBranch) =>
        val condStr = generate(cond)
        val thenStr = generate(thenBranch, indentLevel + 1)
        val elseStr = generate(elseBranch, indentLevel + 1)
        s"${indent}if $condStr then\r\n$thenStr\r\n${indent}else\r\n$elseStr"
      case CodeGenNode.For(cursors, body) =>
        val cursorsStr = cursors
          .map(generate)
          .map(cStr => s"${indent}  $cStr\r\n")
          .mkString
        var bodyStr = generate(body, indentLevel + 1)
        s"${indent}for\r\n$cursorsStr${indent}yield\r\n$bodyStr"
      case CodeGenNode.Throw(excClass, excMessage) =>
        val msgStr = generate(excMessage)
        s"${indent}throw new $excClass($msgStr)"
      case CodeGenNode.Try(tryBody, excClass, catchBody) =>
        val tryStr = generate(tryBody, indentLevel + 1)
        val catchStr = generate(catchBody, indentLevel + 1)
        s"${indent}try\r\n$tryStr\r\n${indent}catch case _: $excClass =>\r\n$catchStr"
      case CodeGenNode.Text(str) => s"${indent}\"$str\""
      case CodeGenNode.FormattedText(formattingStrs*) =>
        formattingStrs
          .map({
            case s: String => s
            case node: CodeGenNode => 
              val nodeStr = generate(node)
              s"$${$nodeStr}"
          })
          .mkString(s"${indent}s\"", "", "\"")
      case CodeGenNode.Integer(n) => s"${indent}$n"
      case CodeGenNode.Boolean(b) => s"${indent}$b"
      case CodeGenNode.Unit => s"${indent}()"
      case CodeGenNode.Not(exp) => s"${indent}!${generate(exp)}"
      case CodeGenNode.Equals(l, r) => s"${indent}${generate(l)} == ${generate(r)}"
      case CodeGenNode.NotEquals(l, r) => s"${indent}${generate(l)} != ${generate(r)}"
      case CodeGenNode.And(l, r) => s"${indent}${generate(l)} && ${generate(r)}"
      case CodeGenNode.Or(l, r) => s"${indent}${generate(l)} || ${generate(r)}"
      case CodeGenNode.Apply(fun, args*) => 
        val funStr = generate(fun, indentLevel)
        val argsStr = args.map(a => indentIfMultiline(generate(a), indentLevel + 1)).mkString(", ")
        s"${funStr}(${argsStr})" 
      case CodeGenNode.Let(name, exp, body) =>
        val expStr = generate(exp)
        val bodyStr = generate(body, indentLevel)
        s"${indent}val $name = $expStr\r\n$bodyStr"
      case CodeGenNode.Var(name) => s"${indent}$name"
      case CodeGenNode.Field(obj, field) =>
        val objStr = generate(obj, indentLevel)
        if objStr.linesIterator.length <= 1 then
          s"$objStr.$field"
        else
          s"${indent}(${indentIfMultiline(objStr, 1)}).$field"
      case CodeGenNode.Match(matchedExp, branches) =>
        val matchedStr = generate(matchedExp, indentLevel)
        val matchesStr = 
          (
            for case (patExp, thenExp) <- branches
            yield
              val patStr = generate(patExp)
              val thenStr = generate(thenExp)
              s"${indent}  case $patStr => $thenStr"
          )
          .mkString("{\r\n", "\r\n", s"\r\n$indent}")

        s"$matchedStr match $matchesStr"
      case CodeGenNode.Return(exp) => s"${indent}return ${generate(exp)}"
    }

  def generate(cgCursor: CodeGenForCursor): String = cgCursor match {
    case CodeGenForCursor.Filter(exp) => s"if ${generate(exp)}"
    case CodeGenForCursor.Iterate(name, collection) => s"$name <- ${generate(collection)}"
    case CodeGenForCursor.Let(name, exp) => s"$name = ${generate(exp)}"
  }
