package tyes.compiler.target

import java.nio.file.Path
import utils.collections.*

private val TCD = TargetCodeDecl
private val TCFC = TargetCodeForCursor
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class ScalaTargetCodeGenerator extends TargetCodeGenerator:

  private def indentIfMultiline(codeStr: String, indentLevel: Int, wrappers: (String, String) = ("\r\n", "\r\n")): String =
    val lines = codeStr.linesWithSeparators.toSeq
    if lines.length <= 1 then
      codeStr
    else if lines(0).trim().last == '{' then
      val indent = "  ".repeat(indentLevel - 2)
      val tailLines =
        lines
          .drop(1)
          .take(lines.length - 1)
          .map(_.prependedAll(indent)).mkString

      lines(0) + tailLines
    else
      val (wrapStart, wrapEnd) = wrappers
      val indent = "  ".repeat(indentLevel)
      lines.map(_.prependedAll(indent)).mkString(wrapStart, "", wrapEnd)

  private def parenthesizeIfSpaced(codeStr: String): String =
    if codeStr.contains(' ') then
      s"($codeStr)"
    else
      codeStr

  def getFileName(tcUnit: TargetCodeUnit): Path = Path.of{tcUnit.name + ".scala"}

  def generate(tcUnit: TargetCodeUnit): String =
    val (imports, others) = tcUnit.decls.partition(d => d.isInstanceOf[TCD.Import])
    val importsStr = imports.map(i => generate(i, 0)).mkString("\r\n")
    val othersStr = others.map(o => generate(o, 0)).mkString("\r\n".repeat(2))
    s"$importsStr\r\n\r\n$othersStr\r\n"

  private def generate(tcDecl: TargetCodeDecl, indentLevel: Int): String = 
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
      case TCD.ADT(name, inherits, cs) =>
        val extendsStr = inherits.map(generate).mkStringOrEmpty(" extends ", ", ", "")
        val caseIndent = s"$indent  "
        val casesStr = caseIndent + cs.map(generate).mkString(s"\r\n${caseIndent}")
        s"${indent}enum ${name}${extendsStr}:\r\n${casesStr}"
      case TCD.Extractor(name, param, retTypeRef, body) =>
        val unapplyMethod = TCD.Method(
          "unapply",
          params = Seq(param),
          retTypeRef = TCTypeRef("Option", retTypeRef),
          body = body(
            n => TCN.ADTConstructorCall(TCTypeRef("Some"), n),
            () => TCN.ADTConstructorCall(TCTypeRef("None"))
          )
        )
        val unapplyStr = generate(unapplyMethod, indentLevel + 1)
        s"${indent}object ${name}:\r\n${unapplyStr}"
    }

  private def generate(tcADTCons: TargetCodeADTConstructor): String =
    val paramsStr = tcADTCons.params
      .map((n, t) => s"$n: ${generate(t)}")
      .mkStringOrEmpty("(", ", ", ")")
    val extendsStr = tcADTCons.inherits.map(generate).mkStringOrEmpty(" extends ", ", ", "")
    s"case ${tcADTCons.name}${paramsStr}${extendsStr}"

  private def generate(tcTypeRef: TargetCodeTypeRef): String =
    val nameStr = (tcTypeRef.namespaces :+ tcTypeRef.name).mkString(".")
    val paramsStr = tcTypeRef.params.map(generate).mkStringOrEmpty("[", ", ", "]")
    nameStr + paramsStr

  def generate(tcNode: TargetCodeNode): String = generate(tcNode, indentLevel = 0)

  def generate(tcNode: TargetCodeNode, indentLevel: Int, skipStartIndent: Boolean = false): String = 
    val indent = "  ".repeat(indentLevel)
    val startIndent = if skipStartIndent then "" else indent
    tcNode match {
      case TCN.If(cond, thenBranch, elseBranch) =>
        val condStr = generate(cond)
        val thenStr = generate(thenBranch, indentLevel + 1)
        val elseStr = elseBranch match {
          case _: TCN.If =>
            " " + generate(elseBranch, indentLevel, skipStartIndent = true)
          case _ =>
            "\r\n" + generate(elseBranch, indentLevel + 1)
        }
        s"${startIndent}if $condStr then\r\n$thenStr\r\n${indent}else$elseStr"
      case TCN.For(cursors, body) =>
        val cursorsStr = cursors
          .map(c => generate(c, indentLevel + 1))
          .map(cStr => s"$cStr\r\n")
          .mkString
        var bodyStr = generate(body, indentLevel + 1)
        s"${startIndent}for\r\n$cursorsStr${indent}yield\r\n$bodyStr"
      case TCN.Throw(excClass, excMessage) =>
        val msgStr = generate(excMessage)
        s"${startIndent}throw new $excClass($msgStr)"
      case TCN.Try(tryBody, excClass, catchBody) =>
        val tryStr = generate(tryBody, indentLevel + 1)
        val catchStr = generate(catchBody, indentLevel + 1)
        s"${startIndent}try\r\n$tryStr\r\n${indent}catch case _: $excClass =>\r\n$catchStr"
      case TCN.Text(str) => s"${startIndent}\"$str\""
      case TCN.FormattedText(formattingStrs*) =>
        formattingStrs
          .map({
            case s: String => s
            case node: TargetCodeNode => 
              val nodeStr = generate(node)
              s"$${$nodeStr}"
          })
          .mkString(s"${startIndent}s\"", "", "\"")
      case TCN.Integer(n) => s"${startIndent}$n"
      case TCN.Boolean(b) => s"${startIndent}$b"
      case TCN.Unit => s"${startIndent}()"
      case TCN.Not(exp) => s"${startIndent}!${generate(exp)}"
      case TCN.Equals(l, r) => s"${startIndent}${generate(l)} == ${generate(r)}"
      case TCN.NotEquals(l, r) => s"${startIndent}${generate(l)} != ${generate(r)}"
      case TCN.And(l, r) => s"${startIndent}${generate(l)} && ${generate(r)}"
      case TCN.Or(l, r) => s"${startIndent}${generate(l)} || ${generate(r)}"
      case TCN.Entry(k, v) => s"${startIndent}${generate(k)} -> ${generate(v)}"
      case TCN.InfixApply(l, fun, r) =>
        val lStr = parenthesizeIfSpaced(generate(l))
        val rStr = parenthesizeIfSpaced(generate(r))
        s"${startIndent}$lStr $fun $rStr"
      case TCN.Apply(fun, args*) => 
        val funStr = generate(fun, indentLevel, skipStartIndent)
        val argsStr = args.map(a => indentIfMultiline(generate(a), indentLevel + 1)).mkString(", ")
        s"${funStr}(${argsStr})"
      case TCN.TypeApply(fun, typeArgs*) =>
        val funStr = generate(fun, indentLevel, skipStartIndent)
        val argsStr = typeArgs.map(a => generate(a)).mkString(", ")
        s"${funStr}[${argsStr}]"
      case TCN.TypeCheck(exp, typeRef) =>
        val expStr = generate(exp, indentLevel, skipStartIndent)
        val typeRefStr = generate(typeRef)
        s"${expStr}.isInstanceOf[${typeRefStr}]"
      case TCN.Let(varPat, exp, body) =>
        val varPatStr = generate(varPat)
        val expStr = generate(exp)
        val bodyStr = generate(body, indentLevel)
        s"${startIndent}val $varPatStr = $expStr\r\n$bodyStr"
      case TCN.Lambda(name, TCN.Match(TCN.Var(name2), bs)) if name == name2 =>
        (
          for case (patExp, thenExp) <- bs
          yield
            val patStr = generate(patExp)
            val thenStr = indentIfMultiline(generate(thenExp), indentLevel + 1)
            s"${indent}  case $patStr => $thenStr"
        )
        .mkString(startIndent + "{\r\n", "\r\n", s"\r\n${indent}" + "}")
      case TCN.Lambda(name, body) =>
        val bodyStr = generate(body, indentLevel + 1, skipStartIndent = true)
        s"${startIndent}$name => $bodyStr"
      case TCN.Var(name) => s"${startIndent}$name"
      case TCN.Field(obj, field) =>
        val objStr = generate(obj, indentLevel, skipStartIndent)
        if objStr.linesIterator.length <= 1 then
          s"$objStr.$field"
        else
          val wrapperIndent = if skipStartIndent then indent else ""
          val wrappers = (s"(\r\n$wrapperIndent", s"\r\n$wrapperIndent)")
          val multiLineObjStr = indentIfMultiline(objStr, 1, wrappers)
          s"${startIndent}$multiLineObjStr.$field"
      case TCN.Match(matchedExp, branches) =>
        val matchedStr = generate(matchedExp, indentLevel, skipStartIndent)
        val matchesStr = 
          (
            for case (patExp, thenExp) <- branches
            yield
              val patStr = generate(patExp)
              val thenStr = indentIfMultiline(generate(thenExp), indentLevel + 2)
              s"${indent}  case $patStr => $thenStr"
          )
          .mkString("{\r\n", "\r\n", s"\r\n$indent}")

        s"$matchedStr match $matchesStr"
      case TCN.Return(exp) => s"${startIndent}return ${generate(exp)}"
      case TCN.ADTConstructorCall(typeRef, args*) =>
        val typeRefStr = generate(typeRef)
        val argsStr = args.map(generate).mkStringOrEmpty("(", ", ", ")")
        startIndent + typeRefStr + argsStr
    }

  def generate(tcCursor: TargetCodeForCursor, indentLevel: Int): String = 
    val indent = "  ".repeat(indentLevel)
    tcCursor match {
      case TCFC.Filter(exp) => 
        val expStr = generate(exp, indentLevel + 1, skipStartIndent = true)
        s"${indent}if $expStr"
      case TCFC.Iterate(pat, collection) =>
        val patStr = generate(pat)
        val colStr = generate(collection, indentLevel + 1, skipStartIndent = true)
        val colIndent = if colStr.linesWithSeparators.length <= 1 then "" else s"\r\n  $indent"
        s"${indent}$patStr <- ${colIndent}${colStr}"
      case TCFC.Let(pat, exp) =>
        val patStr = generate(pat)
        val expStr = generate(exp, indentLevel + 1, skipStartIndent = true) 
        s"${indent}$patStr = $expStr"
    }

  def generate(tcPattern: TargetCodePattern): String = tcPattern match {
    case TCP.Any => "_"
    case TCP.Integer(n) => n.toString
    case TCP.Text(str) => s"\"${str.replace("\"", "\\\"")}\""
    case TCP.Var(name) => name
    case TCP.WithType(pat, typeRef) => s"${generate(pat)}: ${generate(typeRef)}"
    case TCP.ADTConstructor(typeRef, args*) =>
      generate(typeRef) + args
        .map(generate)
        .mkStringOrEmpty("(", ", ", ")")
    case TCP.Extract(extractorName, args*) =>
      extractorName + args.map(generate).mkString("(", ", ", ")")
  }
