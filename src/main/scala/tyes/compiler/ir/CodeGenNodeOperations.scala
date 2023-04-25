package tyes.compiler.ir

import utils.collections.Multiset

object CodeGenNodeOperations extends CodeOperations[CodeGenNode]:

  def negate(code: CodeGenNode): CodeGenNode = code match {
    case CodeGenNode.Equals(l, r) => CodeGenNode.NotEquals(l, r)
    case CodeGenNode.NotEquals(l, r) => CodeGenNode.Equals(l, r)
    case _ => CodeGenNode.Not(code)
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
  
  def applyUntil(cgNode: CodeGenNode, pf: PartialFunction[CodeGenNode, CodeGenNode]): CodeGenNode =
    pf.applyOrElse(cgNode, applyToChildren(_, c => applyUntil(c, pf)))

  def applyToChildren(cgNode: CodeGenNode, f: CodeGenNode => CodeGenNode): CodeGenNode = cgNode match {
    case CodeGenNode.Unit
      | CodeGenNode.Boolean(_)
      | CodeGenNode.Text(_)
      | CodeGenNode.Var(_)
      | CodeGenNode.Integer(_)
      => cgNode
    case CodeGenNode.And(l, r) => CodeGenNode.And(f(l), f(r))
    case CodeGenNode.Apply(fun, args*) => CodeGenNode.Apply(f(fun), args.map(f(_))*)
    case CodeGenNode.Equals(l, r) => CodeGenNode.Equals(f(l), f(r)) 
    case CodeGenNode.Field(obj, field) => CodeGenNode.Field(f(obj), field)
    case CodeGenNode.For(cs, body) => CodeGenNode.For(cs.map(applyToChildren(_, f)), f(body))
    case CodeGenNode.FormattedText(fs*) => CodeGenNode.FormattedText(fs.map({ 
      case n: CodeGenNode => f(n)
      case s: String => s
    })*)
    case CodeGenNode.If(c, t, e) => CodeGenNode.If(f(c), f(t), f(e))
    case CodeGenNode.Let(n, e, b) => CodeGenNode.Let(n, f(e), f(b))
    case CodeGenNode.Match(e, bs) => CodeGenNode.Match(f(e), bs.map((k, v) => (k, f(v))))
    case CodeGenNode.Not(e) => CodeGenNode.Not(f(e))
    case CodeGenNode.NotEquals(l, r) => CodeGenNode.NotEquals(f(l), f(r))
    case CodeGenNode.Or(l, r) => CodeGenNode.Or(f(l), f(r))
    case CodeGenNode.Return(e) => CodeGenNode.Return(f(e))
    case CodeGenNode.Throw(exc, err) => CodeGenNode.Throw(exc, f(err))
    case CodeGenNode.Try(t, exc, c) => CodeGenNode.Try(f(t), exc, f(c))
  }

  def applyToChildren(cgCursor: CodeGenForCursor, f: CodeGenNode => CodeGenNode): CodeGenForCursor = cgCursor match {
    case CodeGenForCursor.Filter(exp) => CodeGenForCursor.Filter(f(exp))
    case CodeGenForCursor.Iterate(name, col) => CodeGenForCursor.Iterate(name, f(col))
    case CodeGenForCursor.Let(name, exp) => CodeGenForCursor.Let(name, f(exp))
  }

  def replace(cgNode: CodeGenNode, key: String, value: CodeGenNode): CodeGenNode = applyUntil(cgNode, {
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

  def simplify(cgNode: CodeGenNode): CodeGenNode = applyUntil(cgNode, {
    case n1 @ CodeGenNode.Let(name1, exp1, n2 @ CodeGenNode.Let(name2, exp2, body)) if name1 != name2 =>
      val exp2FNs = freeNames(exp2).count(name1)
      val bodyFNs = freeNames(body).count(name1)
      if exp2FNs + bodyFNs == 1 then
        if exp2FNs == 1 then
          simplify(CodeGenNode.Let(name2, replace(exp2, name1, exp1), body))
        else
          simplify(CodeGenNode.Let(name2, exp2, replace(body, name1, exp1)))
      else
        n1
  })