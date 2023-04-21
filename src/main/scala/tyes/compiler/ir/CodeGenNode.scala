package tyes.compiler.ir

enum CodeGenNode:
  case If(cond: CodeGenNode, thenBranch: CodeGenNode, elseBranch: CodeGenNode)
  case For(cursors: Seq[CodeGenForCursor], body: CodeGenNode)
  case Throw(excClass: String, excMessage: CodeGenNode)
  case Try(tryBody: CodeGenNode, excClass: String, catchBody: CodeGenNode)
  case Text(str: String)
  case FormattedText(formattingStr: (String | CodeGenNode)*)
  case Integer(n: Int)
  case Boolean(b: scala.Boolean)
  case Unit
  case Var(str: String)
  case Field(obj: CodeGenNode, field: String)
  case Not(exp: CodeGenNode)
  case Equals(left: CodeGenNode, right: CodeGenNode)
  case NotEquals(left: CodeGenNode, right: CodeGenNode)
  case And(left: CodeGenNode, right: CodeGenNode)
  case Or(left: CodeGenNode, right: CodeGenNode)
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

def indentIfMultiline(codeStr: String, indentLevel: Int): String =
  val lines = codeStr.linesWithSeparators.toSeq
  if lines.length <= 1 then
    codeStr
  else
    val indent = "  ".repeat(indentLevel)
    lines.map(_.prependedAll(indent)).mkString("\r\n", "", "\r\n")

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
    case CodeGenNode.Try(tryBody, excClass, catchBody) =>
      val tryStr = compile(tryBody, indentLevel + 1)
      val catchStr = compile(catchBody, indentLevel + 1)
      s"${indent}try\r\n$tryStr\r\n${indent}catch case _: $excClass =>\r\n$catchStr"
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
    case CodeGenNode.Boolean(b) => s"${indent}$b"
    case CodeGenNode.Unit => s"${indent}()"
    case CodeGenNode.Not(exp) => s"${indent}!${compile(exp)}"
    case CodeGenNode.Equals(l, r) => s"${indent}${compile(l)} == ${compile(r)}"
    case CodeGenNode.NotEquals(l, r) => s"${indent}${compile(l)} != ${compile(r)}"
    case CodeGenNode.And(l, r) => s"${indent}${compile(l)} && ${compile(r)}"
    case CodeGenNode.Or(l, r) => s"${indent}${compile(l)} || ${compile(r)}"
    case CodeGenNode.Apply(fun, args*) => 
      val funStr = compile(fun, indentLevel)
      val argsStr = args.map(a => indentIfMultiline(compile(a), indentLevel + 1)).mkString(", ")
      s"${funStr}(${argsStr})" 
    case CodeGenNode.Let(name, exp, body) =>
      val expStr = compile(exp)
      val bodyStr = compile(body, indentLevel)
      s"${indent}val $name = $expStr\r\n$bodyStr"
    case CodeGenNode.Var(name) => s"${indent}$name"
    case CodeGenNode.Field(obj, field) =>
      val objStr = compile(obj, indentLevel)
      if objStr.linesIterator.length <= 1 then
        s"$objStr.$field"
      else
        s"${indent}(${indentIfMultiline(objStr, 1)}).$field"
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

class Multiset[A](private val map: Map[A, Int]):

  import Multiset.*

  def toMap = map

  def +(elem: A) = new Multiset(innerAdd(map, elem, 1))

  def except(elem: A) = new Multiset(map.removed(elem))

  def ++(ms: Multiset[A]) = new Multiset(ms.map.foldLeft(map) { case (m, (e, c)) => innerAdd(m, e, c) })

  def count(elem: A): Int = map.getOrElse(elem, 0)

object Multiset:

  def apply[A](elems: A*): Multiset[A] = new Multiset(elems.groupMapReduce(e => e)(e => 1)(_ + _))

  private def innerAdd[A](map: Map[A, Int], elem: A, count: Int): Map[A, Int] = map.updatedWith(elem) {
    case None => 
      if count > 0 then
        Some(count) 
      else
        None
    case Some(c) => 
      val newTotal = c + count
      if newTotal > 0 then
        Some(newTotal)
      else
        None
  }


def freeNames(cgNode: CodeGenNode): Multiset[String] = cgNode match {
  case CodeGenNode.Var(name) => Multiset(name)
  case CodeGenNode.Let(varName, varExp, bodyExp) => freeNames(varExp) ++ (freeNames(bodyExp).except(varName))
  case CodeGenNode.For(Nil, bodyExp) => freeNames(bodyExp)
  case CodeGenNode.For(CodeGenForCursor.Filter(cond) :: cs, bodyExp) =>
    freeNames(cond) ++ freeNames(CodeGenNode.For(cs, bodyExp))
  case CodeGenNode.For(CodeGenForCursor.Iterate(name, col) :: cs, bodyExp) =>
    freeNames(col) ++ (freeNames(CodeGenNode.For(cs, bodyExp)).except(name))
  case CodeGenNode.For(CodeGenForCursor.Let(name, exp) :: cs, bodyExp) =>
    freeNames(exp) ++ (freeNames(CodeGenNode.For(cs, bodyExp)).except(name))
  case CodeGenNode.FormattedText(fs*) =>
    fs
      .map({ case n: CodeGenNode => freeNames(n) ; case _ => Multiset() })
      .foldLeft(Multiset())(_ ++ _)
  case _ => 
    cgNode
      .productIterator
      .map({ 
        case n: CodeGenNode => freeNames(n)
        case ns: Seq[CodeGenNode] => ns.map(freeNames).foldLeft(Multiset[String]())(_ ++ _)
        case _ => Multiset()
      })
      .foldLeft(Multiset())(_ ++ _)     
}

def map(cgNode: CodeGenNode, pf: PartialFunction[CodeGenNode, CodeGenNode]): CodeGenNode = pf.applyOrElse(cgNode, {
  case CodeGenNode.Unit
    | CodeGenNode.Boolean(_)
    | CodeGenNode.Text(_)
    | CodeGenNode.Var(_)
    | CodeGenNode.Integer(_)
    => cgNode
  case CodeGenNode.And(l, r) => CodeGenNode.And(map(l, pf), map(r, pf))
  case CodeGenNode.Apply(fun, args*) => CodeGenNode.Apply(map(fun, pf), args.map(map(_, pf))*)
  case CodeGenNode.Equals(l, r) => CodeGenNode.Equals(map(l, pf), map(r, pf)) 
  case CodeGenNode.Field(obj, field) => CodeGenNode.Field(map(obj, pf), field)
  case CodeGenNode.For(cs, body) => CodeGenNode.For(cs.map(map(_, pf)), map(body, pf))
  case CodeGenNode.FormattedText(fs*) => CodeGenNode.FormattedText(fs.map({ 
    case n: CodeGenNode => map(n, pf)
    case s: String => s
  })*)
  case CodeGenNode.If(c, t, e) => CodeGenNode.If(map(c, pf), map(t, pf), map(e, pf))
  case CodeGenNode.Let(n, e, b) => CodeGenNode.Let(n, map(e, pf), map(b, pf))
  case CodeGenNode.Match(e, bs) => CodeGenNode.Match(map(e, pf), bs.map((k, v) => (k, map(v, pf))))
  case CodeGenNode.Not(e) => CodeGenNode.Not(map(e, pf))
  case CodeGenNode.NotEquals(l, r) => CodeGenNode.NotEquals(map(l, pf), map(r, pf))
  case CodeGenNode.Or(l, r) => CodeGenNode.Or(map(l, pf), map(r, pf))
  case CodeGenNode.Return(e) => CodeGenNode.Return(map(e, pf))
  case CodeGenNode.Throw(exc, err) => CodeGenNode.Throw(exc, map(err, pf))
  case CodeGenNode.Try(t, exc, c) => CodeGenNode.Try(map(t, pf), exc, map(c, pf))
})

def map(cgCursor: CodeGenForCursor, pf: PartialFunction[CodeGenNode, CodeGenNode]): CodeGenForCursor = cgCursor match {
  case CodeGenForCursor.Filter(exp) => CodeGenForCursor.Filter(map(exp, pf))
  case CodeGenForCursor.Iterate(name, col) => CodeGenForCursor.Iterate(name, map(col, pf))
  case CodeGenForCursor.Let(name, exp) => CodeGenForCursor.Let(name, map(exp, pf))
}

def replace(cgNode: CodeGenNode, key: String, value: CodeGenNode): CodeGenNode = map(cgNode, {
  case CodeGenNode.Var(name) if name == key => value 
  case CodeGenNode.Let(varName, varExp, bodyExp) =>
    val newBodyExp = if varName == key then bodyExp else replace(bodyExp, key, value)
    CodeGenNode.Let(varName, replace(varExp, key, value), newBodyExp) 
  case CodeGenNode.For(Nil, bodyExp) => CodeGenNode.For(Nil, replace(bodyExp, key, value))
  case CodeGenNode.For(CodeGenForCursor.Filter(cond) :: cs, bodyExp) =>
    val CodeGenNode.For(newCs, newBodyExp) = replace(CodeGenNode.For(cs, bodyExp), key, value)
    val c = CodeGenForCursor.Filter(replace(cond, key, value))
    CodeGenNode.For(c +: newCs, newBodyExp)
  case CodeGenNode.For(CodeGenForCursor.Iterate(name, col) :: cs, bodyExp) =>
    val CodeGenNode.For(newCs, newBodyExp) = 
      if name == key then 
        CodeGenNode.For(cs, bodyExp) 
      else
        replace(CodeGenNode.For(cs, bodyExp), key, value)
    val c = CodeGenForCursor.Iterate(name, replace(col, key, value))
    CodeGenNode.For(c +: newCs, newBodyExp)
  case CodeGenNode.For(CodeGenForCursor.Let(name, exp) :: cs, bodyExp) =>
    val CodeGenNode.For(newCs, newBodyExp) = 
      if name == key then 
        CodeGenNode.For(cs, bodyExp) 
      else
        replace(CodeGenNode.For(cs, bodyExp), key, value)
    val c = CodeGenForCursor.Let(name, replace(exp, key, value))
    CodeGenNode.For(c +: newCs, newBodyExp)
})

def simplify(cgNode: CodeGenNode): CodeGenNode = map(cgNode, {
  case CodeGenNode.Let(name1, exp1, n2 @ CodeGenNode.Let(name2, exp2, body)) if name1 != name2 =>
    val exp2FNs = freeNames(exp2).count(name1)
    val bodyFNs = freeNames(body).count(name1)
    if exp2FNs + bodyFNs == 1 then
      if exp2FNs == 1 then
        simplify(CodeGenNode.Let(name2, replace(exp2, name1, exp1), body))
      else
        simplify(CodeGenNode.Let(name2, exp2, replace(body, name1, exp1)))
    else
      CodeGenNode.Let(name1, simplify(exp1), simplify(n2))
})