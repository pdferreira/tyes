package tyes.compiler.ir

import java.nio.file.Path
import utils.collections.*

val TCD = TargetCodeDecl

class ScalaTargetCodeGenerator extends TargetCodeGenerator:
  
  private def indentIfMultiline(codeStr: String, indentLevel: Int): String =
    val lines = codeStr.linesWithSeparators.toSeq
    if lines.length <= 1 then
      codeStr
    else
      val indent = "  ".repeat(indentLevel)
      lines.map(_.prependedAll(indent)).mkString("\r\n", "", "\r\n")

  def getFileName(tcUnit: TargetCodeUnit): Path = Path.of{tcUnit.name + ".scala"}

  def generate(tcUnit: TargetCodeUnit): String =
    val (imports, others) = tcUnit.decls.partition(d => d.isInstanceOf[TargetCodeDecl.Import])
    val importsStr = imports.map(i => generate(i, 0)).mkString("\r\n")
    val othersStr = others.map(o => generate(o, 0)).mkString("\r\n".repeat(2))
    s"$importsStr\r\n\r\n$othersStr\r\n"

  def generate(tcDecl: TargetCodeDecl, indentLevel: Int): String = 
    val indent = "  ".repeat(indentLevel)
    tcDecl match {
      case TCD.Import(ns, all) =>
        val nsStr = ns.mkString(".") + (if all then ".*" else "")
        s"${indent}import $nsStr"
      case TCD.Type(alias, tn) =>
        s"${indent}type $alias = ${generate(tn)}"
      case TCD.Class(name, inherits, decls) =>
        val extendsStr = inherits.map(generate).mkStringOrEmpty(" extends ", ", ", "") 
        val declsStr = decls.map(generate(_, indentLevel + 1)).mkString("\r\n".repeat(2))
        s"${indent}class ${name}${extendsStr}:\r\n${declsStr}"
      case TCD.Method(name, params, rtName, body) =>
        val paramsStr = params.map((n, t) => s"$n: ${generate(t)}").mkString(", ")
        val bodyStr = generate(body, indentLevel, skipStartIndent = true)
        s"${indent}def $name($paramsStr): ${generate(rtName)} = $bodyStr"
    }

  def generate(tcTypeRef: TargetCodeTypeRef): String =
    val nameStr = (tcTypeRef.namespaces :+ tcTypeRef.name).mkString(".")
    val paramsStr = tcTypeRef.params.map(generate).mkStringOrEmpty("[", ", ", "]")
    nameStr + paramsStr

  def generate(tcNode: TargetCodeNode): String = generate(tcNode, indentLevel = 0)

  def generate(tcNode: TargetCodeNode, indentLevel: Int, skipStartIndent: Boolean = false): String = 
    val indent = "  ".repeat(indentLevel)
    val startIndent = if skipStartIndent then "" else indent
    tcNode match {
      case TargetCodeNode.If(cond, thenBranch, elseBranch) =>
        val condStr = generate(cond)
        val thenStr = generate(thenBranch, indentLevel + 1)
        val elseStr = generate(elseBranch, indentLevel + 1)
        s"${startIndent}if $condStr then\r\n$thenStr\r\n${indent}else\r\n$elseStr"
      case TargetCodeNode.For(cursors, body) =>
        val cursorsStr = cursors
          .map(generate)
          .map(cStr => s"${indent}  $cStr\r\n")
          .mkString
        var bodyStr = generate(body, indentLevel + 1)
        s"${startIndent}for\r\n$cursorsStr${indent}yield\r\n$bodyStr"
      case TargetCodeNode.Throw(excClass, excMessage) =>
        val msgStr = generate(excMessage)
        s"${startIndent}throw new $excClass($msgStr)"
      case TargetCodeNode.Try(tryBody, excClass, catchBody) =>
        val tryStr = generate(tryBody, indentLevel + 1)
        val catchStr = generate(catchBody, indentLevel + 1)
        s"${startIndent}try\r\n$tryStr\r\n${indent}catch case _: $excClass =>\r\n$catchStr"
      case TargetCodeNode.Text(str) => s"${startIndent}\"$str\""
      case TargetCodeNode.FormattedText(formattingStrs*) =>
        formattingStrs
          .map({
            case s: String => s
            case node: TargetCodeNode => 
              val nodeStr = generate(node)
              s"$${$nodeStr}"
          })
          .mkString(s"${startIndent}s\"", "", "\"")
      case TargetCodeNode.Integer(n) => s"${startIndent}$n"
      case TargetCodeNode.Boolean(b) => s"${startIndent}$b"
      case TargetCodeNode.Unit => s"${startIndent}()"
      case TargetCodeNode.Not(exp) => s"${startIndent}!${generate(exp)}"
      case TargetCodeNode.Equals(l, r) => s"${startIndent}${generate(l)} == ${generate(r)}"
      case TargetCodeNode.NotEquals(l, r) => s"${startIndent}${generate(l)} != ${generate(r)}"
      case TargetCodeNode.And(l, r) => s"${startIndent}${generate(l)} && ${generate(r)}"
      case TargetCodeNode.Or(l, r) => s"${startIndent}${generate(l)} || ${generate(r)}"
      case TargetCodeNode.Apply(fun, args*) => 
        val funStr = generate(fun, indentLevel, skipStartIndent)
        val argsStr = args.map(a => indentIfMultiline(generate(a), indentLevel + 1)).mkString(", ")
        s"${funStr}(${argsStr})" 
      case TargetCodeNode.Let(name, exp, body) =>
        val expStr = generate(exp)
        val bodyStr = generate(body, indentLevel)
        s"${startIndent}val $name = $expStr\r\n$bodyStr"
      case TargetCodeNode.Var(name) => s"${startIndent}$name"
      case TargetCodeNode.Field(obj, field) =>
        val objStr = generate(obj, indentLevel, skipStartIndent)
        if objStr.linesIterator.length <= 1 then
          s"$objStr.$field"
        else
          s"${startIndent}(${indentIfMultiline(objStr, 1)}).$field"
      case TargetCodeNode.Match(matchedExp, branches) =>
        val matchedStr = generate(matchedExp, indentLevel, skipStartIndent)
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
      case TargetCodeNode.Return(exp) => s"${startIndent}return ${generate(exp)}"
    }

  def generate(tcCursor: TargetCodeForCursor): String = tcCursor match {
    case TargetCodeForCursor.Filter(exp) => s"if ${generate(exp)}"
    case TargetCodeForCursor.Iterate(name, collection) => s"$name <- ${generate(collection)}"
    case TargetCodeForCursor.Let(name, exp) => s"$name = ${generate(exp)}"
  }
