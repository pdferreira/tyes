package tyes.compiler.target

import utils.collections.Multiset

private val TCD = TargetCodeDecl
private val TCFC = TargetCodeForCursor
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern

object TargetCodeNodeOperations extends CodeOperations:

  def negate(code: TargetCodeNode): TargetCodeNode = code match {
    case TCN.Equals(l, r) => TCN.NotEquals(l, r)
    case TCN.NotEquals(l, r) => TCN.Equals(l, r)
    case _ => TCN.Not(code)
  }

  def freeNames(tcNode: TargetCodeNode): Multiset[String] = tcNode match {
    case TCN.Var(name) => Multiset(name)
    case TCN.Let(varPat, varExp, bodyExp) => freeNames(varExp) ++ (freeNames(bodyExp).except(boundNames(varPat)))
    case TCN.Lambda(name, exp) => freeNames(exp).except(name)
    case TCN.For(Nil, bodyExp) => freeNames(bodyExp)
    case TCN.For(TCFC.Filter(cond) :: cs, bodyExp) =>
      freeNames(cond) ++ freeNames(TCN.For(cs, bodyExp))
    case TCN.For(TCFC.Iterate(pat, col) :: cs, bodyExp) =>
      freeNames(col) ++ (freeNames(TCN.For(cs, bodyExp)).except(boundNames(pat)))
    case TCN.For(TCFC.Let(pat, exp) :: cs, bodyExp) =>
      freeNames(exp) ++ (freeNames(TCN.For(cs, bodyExp)).except(boundNames(pat)))
    case TCN.FormattedText(fs*) =>
      fs
        .map({ case n: TargetCodeNode => freeNames(n) ; case _ => Multiset() })
        .foldLeft(Multiset())(_ ++ _)
    case TCN.Match(exp, bs) =>
      bs
        .map((p, b) => freeNames(b).except(boundNames(p)))
        .foldLeft(freeNames(exp))(_ ++ _)
    case _ => 
      tcNode
        .productIterator
        .map({ 
          case n: TargetCodeNode => freeNames(n)
          case ns: Seq[Any] if !ns.isEmpty && ns.head.isInstanceOf[TargetCodeNode] =>
            ns
              .asInstanceOf[Seq[TargetCodeNode]]
              .map(freeNames)
              .foldLeft(Multiset[String]())(_ ++ _)
          case _ => Multiset()
        })
        .foldLeft(Multiset())(_ ++ _)     
  }

  def boundNames(tcPattern: TargetCodePattern): Set[String] = tcPattern match {
    case TCP.Any
      | TCP.Integer(_)
      | TCP.Text(_)
      => Set()
    case TCP.Var(name) => Set(name)
    case TCP.WithType(pat, _) => boundNames(pat)
    case TCP.ADTConstructor(_, args*) =>
      args
        .map(boundNames)
        .foldLeft(Set())(_ ++ _)
  }
  
  def applyUntil(tcNode: TargetCodeNode, pf: PartialFunction[TargetCodeNode, TargetCodeNode]): TargetCodeNode =
    pf.applyOrElse(tcNode, applyToChildren(_, c => applyUntil(c, pf)))

  def applyToChildren(tcDecl: TargetCodeDecl, f: TargetCodeNode => TargetCodeNode): TargetCodeDecl = tcDecl match {
    case adt: TCD.ADT => adt.copy(
      constructors = 
        for c <- adt.constructors 
        yield c.copy(
          inherits = 
            for TCN.ADTConstructorCall(tr, args*) <- c.inherits 
            yield TCN.ADTConstructorCall(tr, args.map(f)*)
        )
    )
    case clazz: TCD.Class => clazz.copy(
      decls = clazz.decls.map(applyToChildren(_, f))
    )
    case method: TCD.Method => method.copy(body = f(method.body))
    case _: TCD.Import 
      |  _: TCD.Type
      => tcDecl
  }

  def applyToChildren(tcNode: TargetCodeNode, f: TargetCodeNode => TargetCodeNode): TargetCodeNode = tcNode match {
    case TCN.Unit
      | TCN.Boolean(_)
      | TCN.Text(_)
      | TCN.Var(_)
      | TCN.Integer(_)
      => tcNode
    case TCN.And(l, r) => TCN.And(f(l), f(r))
    case TCN.Entry(k, v) => TCN.Entry(f(k), f(v))
    case TCN.Apply(fun, args*) => TCN.Apply(f(fun), args.map(f(_))*)
    case TCN.InfixApply(l, fun, r) => TCN.InfixApply(f(l), fun, f(r))
    case TCN.TypeApply(fun, ts*) => TCN.TypeApply(f(fun), ts*)
    case TCN.TypeCheck(e, tr) => TCN.TypeCheck(f(e), tr) 
    case TCN.Equals(l, r) => TCN.Equals(f(l), f(r)) 
    case TCN.Field(obj, field) => TCN.Field(f(obj), field)
    case TCN.For(cs, body) => TCN.For(cs.map(applyToChildren(_, f)), f(body))
    case TCN.FormattedText(fs*) => TCN.FormattedText(fs.map({ 
      case n: TargetCodeNode => f(n)
      case s: String => s
    })*)
    case TCN.If(c, t, e) => TCN.If(f(c), f(t), f(e))
    case TCN.Let(n, e, b) => TCN.Let(n, f(e), f(b))
    case TCN.Lambda(n, e) => TCN.Lambda(n, f(e))
    case TCN.Match(e, bs) => TCN.Match(f(e), bs.map((k, v) => (k, f(v))))
    case TCN.Not(e) => TCN.Not(f(e))
    case TCN.NotEquals(l, r) => TCN.NotEquals(f(l), f(r))
    case TCN.Or(l, r) => TCN.Or(f(l), f(r))
    case TCN.Return(e) => TCN.Return(f(e))
    case TCN.Throw(exc, err) => TCN.Throw(exc, f(err))
    case TCN.Try(t, exc, c) => TCN.Try(f(t), exc, f(c))
    case TCN.ADTConstructorCall(tr, args*) => TCN.ADTConstructorCall(tr, args.map(f)*)
  }

  def applyToChildren(tcCursor: TargetCodeForCursor, f: TargetCodeNode => TargetCodeNode): TargetCodeForCursor = tcCursor match {
    case TCFC.Filter(exp) => TCFC.Filter(f(exp))
    case TCFC.Iterate(name, col) => TCFC.Iterate(name, f(col))
    case TCFC.Let(name, exp) => TCFC.Let(name, f(exp))
  }

  def applyToChildren(tcPat: TargetCodePattern, f: TargetCodePattern => TargetCodePattern): TargetCodePattern = tcPat match {
    case TCP.Any => tcPat
    case TCP.Text(_) => tcPat
    case TCP.Integer(_) => tcPat
    case TCP.Var(_) => tcPat
    case TCP.ADTConstructor(typeRef, args*) => TCP.ADTConstructor(typeRef, args.map(f)*)
    case TCP.WithType(pat, typeRef) => TCP.WithType(f(pat), typeRef)
  }

  def replace(tcNode: TargetCodeNode, key: String, value: TargetCodeNode): TargetCodeNode = applyUntil(tcNode, {
    case TCN.Var(name) if name == key => value 
    case TCN.Let(varPat, varExp, bodyExp) =>
      val newBodyExp = if boundNames(varPat).contains(key) then bodyExp else replace(bodyExp, key, value)
      TCN.Let(varPat, replace(varExp, key, value), newBodyExp)
    case TCN.Lambda(paramName, bodyExp) =>
      val newBodyExp = if paramName == key then bodyExp else replace(bodyExp, key, value)
      TCN.Lambda(paramName, newBodyExp)
    case TCN.For(Nil, bodyExp) => TCN.For(Nil, replace(bodyExp, key, value))
    case TCN.For(TCFC.Filter(cond) :: cs, bodyExp) =>
      val TCN.For(newCs, newBodyExp) = replace(TCN.For(cs, bodyExp), key, value)
      val c = TCFC.Filter(replace(cond, key, value))
      TCN.For(c +: newCs, newBodyExp)
    case TCN.For(TCFC.Iterate(pat, col) :: cs, bodyExp) =>
      val TCN.For(newCs, newBodyExp) = 
        if boundNames(pat).contains(key) then 
          TCN.For(cs, bodyExp) 
        else
          replace(TCN.For(cs, bodyExp), key, value)
      val c = TCFC.Iterate(pat, replace(col, key, value))
      TCN.For(c +: newCs, newBodyExp)
    case TCN.For(TCFC.Let(pat, exp) :: cs, bodyExp) =>
      val TCN.For(newCs, newBodyExp) = 
        if boundNames(pat).contains(key) then 
          TCN.For(cs, bodyExp) 
        else
          replace(TCN.For(cs, bodyExp), key, value)
      val c = TCFC.Let(pat, replace(exp, key, value))
      TCN.For(c +: newCs, newBodyExp)
    case TCN.Match(matchedExp, bs) =>
      TCN.Match(
        replace(matchedExp, key, value),
        for (p, b) <- bs yield 
          if boundNames(p).contains(key) then
            p -> b
          else
            p -> replace(b, key, value)
      )
  })

  def isComposite(tcp: TargetCodePattern): Boolean = tcp match {
    case 
      TCP.Any |
      TCP.Var(_) |
      TCP.Text(_) |
      TCP.Integer(_)
      => false
    case _ => true
  }
