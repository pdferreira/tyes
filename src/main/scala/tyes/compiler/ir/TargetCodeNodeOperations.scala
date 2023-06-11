package tyes.compiler.ir

import utils.collections.Multiset

object TargetCodeNodeOperations extends CodeOperations[TargetCodeNode]:

  def negate(code: TargetCodeNode): TargetCodeNode = code match {
    case TargetCodeNode.Equals(l, r) => TargetCodeNode.NotEquals(l, r)
    case TargetCodeNode.NotEquals(l, r) => TargetCodeNode.Equals(l, r)
    case _ => TargetCodeNode.Not(code)
  }

  def freeNames(tcNode: TargetCodeNode): Multiset[String] = tcNode match {
    case TargetCodeNode.Var(name) => Multiset(name)
    case TargetCodeNode.Let(varName, varExp, bodyExp) => freeNames(varExp) ++ (freeNames(bodyExp).except(varName))
    case TargetCodeNode.Lambda(name, exp) => freeNames(exp).except(name)
    case TargetCodeNode.For(Nil, bodyExp) => freeNames(bodyExp)
    case TargetCodeNode.For(TargetCodeForCursor.Filter(cond) :: cs, bodyExp) =>
      freeNames(cond) ++ freeNames(TargetCodeNode.For(cs, bodyExp))
    case TargetCodeNode.For(TargetCodeForCursor.Iterate(name, col) :: cs, bodyExp) =>
      freeNames(col) ++ (freeNames(TargetCodeNode.For(cs, bodyExp)).except(name))
    case TargetCodeNode.For(TargetCodeForCursor.Let(name, exp) :: cs, bodyExp) =>
      freeNames(exp) ++ (freeNames(TargetCodeNode.For(cs, bodyExp)).except(name))
    case TargetCodeNode.FormattedText(fs*) =>
      fs
        .map({ case n: TargetCodeNode => freeNames(n) ; case _ => Multiset() })
        .foldLeft(Multiset())(_ ++ _)
    case TargetCodeNode.Match(exp, bs) =>
      bs
        .map((p, b) => freeNames(b).except(freeNames(p)))
        .foldLeft(freeNames(exp))(_ ++ _)
    case _ => 
      tcNode
        .productIterator
        .map({ 
          case n: TargetCodeNode => freeNames(n)
          case ns: Seq[TargetCodeNode] => ns.map(freeNames).foldLeft(Multiset[String]())(_ ++ _)
          case _ => Multiset()
        })
        .foldLeft(Multiset())(_ ++ _)     
  }
  
  def applyUntil(tcNode: TargetCodeNode, pf: PartialFunction[TargetCodeNode, TargetCodeNode]): TargetCodeNode =
    pf.applyOrElse(tcNode, applyToChildren(_, c => applyUntil(c, pf)))

  def applyToChildren(tcNode: TargetCodeNode, f: TargetCodeNode => TargetCodeNode): TargetCodeNode = tcNode match {
    case TargetCodeNode.Unit
      | TargetCodeNode.Boolean(_)
      | TargetCodeNode.Text(_)
      | TargetCodeNode.Var(_)
      | TargetCodeNode.Integer(_)
      => tcNode
    case TargetCodeNode.And(l, r) => TargetCodeNode.And(f(l), f(r))
    case TargetCodeNode.Entry(k, v) => TargetCodeNode.Entry(f(k), f(v))
    case TargetCodeNode.Apply(fun, args*) => TargetCodeNode.Apply(f(fun), args.map(f(_))*)
    case TargetCodeNode.InfixApply(l, fun, r) => TargetCodeNode.InfixApply(f(l), fun, f(r))
    case TargetCodeNode.Equals(l, r) => TargetCodeNode.Equals(f(l), f(r)) 
    case TargetCodeNode.Field(obj, field) => TargetCodeNode.Field(f(obj), field)
    case TargetCodeNode.For(cs, body) => TargetCodeNode.For(cs.map(applyToChildren(_, f)), f(body))
    case TargetCodeNode.FormattedText(fs*) => TargetCodeNode.FormattedText(fs.map({ 
      case n: TargetCodeNode => f(n)
      case s: String => s
    })*)
    case TargetCodeNode.If(c, t, e) => TargetCodeNode.If(f(c), f(t), f(e))
    case TargetCodeNode.Let(n, e, b) => TargetCodeNode.Let(n, f(e), f(b))
    case TargetCodeNode.Lambda(n, e) => TargetCodeNode.Lambda(n, f(e))
    case TargetCodeNode.Match(e, bs) => TargetCodeNode.Match(f(e), bs.map((k, v) => (k, f(v))))
    case TargetCodeNode.Not(e) => TargetCodeNode.Not(f(e))
    case TargetCodeNode.NotEquals(l, r) => TargetCodeNode.NotEquals(f(l), f(r))
    case TargetCodeNode.Or(l, r) => TargetCodeNode.Or(f(l), f(r))
    case TargetCodeNode.Return(e) => TargetCodeNode.Return(f(e))
    case TargetCodeNode.Throw(exc, err) => TargetCodeNode.Throw(exc, f(err))
    case TargetCodeNode.Try(t, exc, c) => TargetCodeNode.Try(f(t), exc, f(c))
    case TargetCodeNode.ADTConstructorCall(tr, args*) => TargetCodeNode.ADTConstructorCall(tr, args.map(f)*)
  }

  def applyToChildren(tcCursor: TargetCodeForCursor, f: TargetCodeNode => TargetCodeNode): TargetCodeForCursor = tcCursor match {
    case TargetCodeForCursor.Filter(exp) => TargetCodeForCursor.Filter(f(exp))
    case TargetCodeForCursor.Iterate(name, col) => TargetCodeForCursor.Iterate(name, f(col))
    case TargetCodeForCursor.Let(name, exp) => TargetCodeForCursor.Let(name, f(exp))
  }

  def replace(tcNode: TargetCodeNode, key: String, value: TargetCodeNode): TargetCodeNode = applyUntil(tcNode, {
    case TargetCodeNode.Var(name) if name == key => value 
    case TargetCodeNode.Let(varName, varExp, bodyExp) =>
      val newBodyExp = if varName == key then bodyExp else replace(bodyExp, key, value)
      TargetCodeNode.Let(varName, replace(varExp, key, value), newBodyExp)
    case TargetCodeNode.Lambda(paramName, bodyExp) =>
      val newBodyExp = if paramName == key then bodyExp else replace(bodyExp, key, value)
      TargetCodeNode.Lambda(paramName, newBodyExp)
    case TargetCodeNode.For(Nil, bodyExp) => TargetCodeNode.For(Nil, replace(bodyExp, key, value))
    case TargetCodeNode.For(TargetCodeForCursor.Filter(cond) :: cs, bodyExp) =>
      val TargetCodeNode.For(newCs, newBodyExp) = replace(TargetCodeNode.For(cs, bodyExp), key, value)
      val c = TargetCodeForCursor.Filter(replace(cond, key, value))
      TargetCodeNode.For(c +: newCs, newBodyExp)
    case TargetCodeNode.For(TargetCodeForCursor.Iterate(name, col) :: cs, bodyExp) =>
      val TargetCodeNode.For(newCs, newBodyExp) = 
        if name == key then 
          TargetCodeNode.For(cs, bodyExp) 
        else
          replace(TargetCodeNode.For(cs, bodyExp), key, value)
      val c = TargetCodeForCursor.Iterate(name, replace(col, key, value))
      TargetCodeNode.For(c +: newCs, newBodyExp)
    case TargetCodeNode.For(TargetCodeForCursor.Let(name, exp) :: cs, bodyExp) =>
      val TargetCodeNode.For(newCs, newBodyExp) = 
        if name == key then 
          TargetCodeNode.For(cs, bodyExp) 
        else
          replace(TargetCodeNode.For(cs, bodyExp), key, value)
      val c = TargetCodeForCursor.Let(name, replace(exp, key, value))
      TargetCodeNode.For(c +: newCs, newBodyExp)
  })

  def simplify(tcNode: TargetCodeNode): TargetCodeNode = applyUntil(tcNode, {
    case n1 @ TargetCodeNode.Let(name1, exp1, n2 @ TargetCodeNode.Let(name2, exp2, body)) if name1 != name2 =>
      val exp2FNs = freeNames(exp2).count(name1)
      val bodyFNs = freeNames(body).count(name1)
      if exp2FNs + bodyFNs == 1 then
        if exp2FNs == 1 then
          simplify(TargetCodeNode.Let(name2, replace(exp2, name1, exp1), body))
        else
          simplify(TargetCodeNode.Let(name2, exp2, replace(body, name1, exp1)))
      else
        n1
  })