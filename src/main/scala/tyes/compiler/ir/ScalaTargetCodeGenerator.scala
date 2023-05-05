package tyes.compiler.ir

class ScalaTargetCodeGenerator extends TargetCodeGenerator:
  
  private def indentIfMultiline(codeStr: String, indentLevel: Int): String =
    val lines = codeStr.linesWithSeparators.toSeq
    if lines.length <= 1 then
      codeStr
    else
      val indent = "  ".repeat(indentLevel)
      lines.map(_.prependedAll(indent)).mkString("\r\n", "", "\r\n")

  def generate(tcNode: TargetCodeNode): String = generate(tcNode, indentLevel = 0)

  def generate(tcNode: TargetCodeNode, indentLevel: Int): String = 
    val indent = "  ".repeat(indentLevel)
    tcNode match {
      case TargetCodeNode.If(cond, thenBranch, elseBranch) =>
        val condStr = generate(cond)
        val thenStr = generate(thenBranch, indentLevel + 1)
        val elseStr = generate(elseBranch, indentLevel + 1)
        s"${indent}if $condStr then\r\n$thenStr\r\n${indent}else\r\n$elseStr"
      case TargetCodeNode.For(cursors, body) =>
        val cursorsStr = cursors
          .map(generate)
          .map(cStr => s"${indent}  $cStr\r\n")
          .mkString
        var bodyStr = generate(body, indentLevel + 1)
        s"${indent}for\r\n$cursorsStr${indent}yield\r\n$bodyStr"
      case TargetCodeNode.Throw(excClass, excMessage) =>
        val msgStr = generate(excMessage)
        s"${indent}throw new $excClass($msgStr)"
      case TargetCodeNode.Try(tryBody, excClass, catchBody) =>
        val tryStr = generate(tryBody, indentLevel + 1)
        val catchStr = generate(catchBody, indentLevel + 1)
        s"${indent}try\r\n$tryStr\r\n${indent}catch case _: $excClass =>\r\n$catchStr"
      case TargetCodeNode.Text(str) => s"${indent}\"$str\""
      case TargetCodeNode.FormattedText(formattingStrs*) =>
        formattingStrs
          .map({
            case s: String => s
            case node: TargetCodeNode => 
              val nodeStr = generate(node)
              s"$${$nodeStr}"
          })
          .mkString(s"${indent}s\"", "", "\"")
      case TargetCodeNode.Integer(n) => s"${indent}$n"
      case TargetCodeNode.Boolean(b) => s"${indent}$b"
      case TargetCodeNode.Unit => s"${indent}()"
      case TargetCodeNode.Not(exp) => s"${indent}!${generate(exp)}"
      case TargetCodeNode.Equals(l, r) => s"${indent}${generate(l)} == ${generate(r)}"
      case TargetCodeNode.NotEquals(l, r) => s"${indent}${generate(l)} != ${generate(r)}"
      case TargetCodeNode.And(l, r) => s"${indent}${generate(l)} && ${generate(r)}"
      case TargetCodeNode.Or(l, r) => s"${indent}${generate(l)} || ${generate(r)}"
      case TargetCodeNode.Apply(fun, args*) => 
        val funStr = generate(fun, indentLevel)
        val argsStr = args.map(a => indentIfMultiline(generate(a), indentLevel + 1)).mkString(", ")
        s"${funStr}(${argsStr})" 
      case TargetCodeNode.Let(name, exp, body) =>
        val expStr = generate(exp)
        val bodyStr = generate(body, indentLevel)
        s"${indent}val $name = $expStr\r\n$bodyStr"
      case TargetCodeNode.Var(name) => s"${indent}$name"
      case TargetCodeNode.Field(obj, field) =>
        val objStr = generate(obj, indentLevel)
        if objStr.linesIterator.length <= 1 then
          s"$objStr.$field"
        else
          s"${indent}(${indentIfMultiline(objStr, 1)}).$field"
      case TargetCodeNode.Match(matchedExp, branches) =>
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
      case TargetCodeNode.Return(exp) => s"${indent}return ${generate(exp)}"
    }

  def generate(tcCursor: TargetCodeForCursor): String = tcCursor match {
    case TargetCodeForCursor.Filter(exp) => s"if ${generate(exp)}"
    case TargetCodeForCursor.Iterate(name, collection) => s"$name <- ${generate(collection)}"
    case TargetCodeForCursor.Let(name, exp) => s"$name = ${generate(exp)}"
  }
