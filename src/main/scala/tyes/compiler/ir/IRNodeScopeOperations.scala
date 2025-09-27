package tyes.compiler.ir

import utils.collections.Multiset
import tyes.compiler.target.{TargetCodeNodeOperations as TCNOps}

object IRNodeScopeOperations:

  def freeNames(irNode: IRNode): Multiset[String] = irNode match {
    case IRNode.Unexpected => Multiset()
    case IRNode.Error(err) => freeNames(err)
    case IRNode.Type(typExp) => freeNames(typExp)
    case IRNode.Range(colVar, _, seed, cursor, body) =>
      Multiset(colVar) ++ TCNOps.freeNames(seed) ++ freeNames(body).except(cursor)
    case IRNode.Or(main, alt) => freeNames(main) ++ freeNames(alt)
    case IRNode.And(c +: cs, n) =>
      freeNames(c) ++ freeNames(IRNode.And(cs, n)).except(boundNames(c))
    case IRNode.And(_, n) => freeNames(n)
    case IRNode.Switch(bs, o) =>
      bs.map(b => freeNames(b._2)).fold(Multiset())(_ ++ _)
      ++
      freeNames(o)
  }

  def freeNames(cond: IRCond): Multiset[String] = cond match {
    case IRCond.And(left, right) => freeNames(left) ++ freeNames(right)
    case IRCond.EnvSizeIs(envVar, _) => Multiset(envVar)
    case IRCond.OfType(termCode, _) => TCNOps.freeNames(termCode)
    case IRCond.TermEquals(t1Code, t2Code) => TCNOps.freeNames(t1Code) ++ TCNOps.freeNames(t2Code)
    case IRCond.TypeEquals(t1Code, t2Code) => TCNOps.freeNames(t1Code) ++ TCNOps.freeNames(t2Code)
    case IRCond.TypeDecl(_, typExp, expect) => freeNames(typExp) ++ expect.map(freeNames).getOrElse(Multiset()) 
  }

  def freeNames(typExp: IRType): Multiset[String] = typExp match {
    case IRType.EnvGet(envVar, keyCode) => Multiset(envVar) ++ TCNOps.freeNames(keyCode)
    case IRType.FromCode(code, _) => TCNOps.freeNames(code)
    case IRType.Induction(expCode, envCode) => TCNOps.freeNames(expCode) ++ TCNOps.freeNames(envCode)
  }

  def freeNames(expect: IRTypeExpect): Multiset[String] = expect match {
    case IRTypeExpect.EqualsTo(typCode) => TCNOps.freeNames(typCode)
    case IRTypeExpect.OfType(_) => Multiset()
  }

  def freeNames(irError: IRError): Multiset[String] = irError match {
    case IRError.AllOf(errors*) => 
      errors
        .map(freeNames)
        .fold(Multiset())(_ ++ _)
    
    case IRError.OneOf(errors*) => 
      errors
        .map(freeNames)
        .fold(Multiset())(_ ++ _)

    case IRError.Generic(msg) => TCNOps.freeNames(msg)

    case IRError.NoType(exp) => TCNOps.freeNames(exp)

    case IRError.UnexpectedEnvSize(env, _) => TCNOps.freeNames(env)

    case IRError.UnexpectedType(obt, exp) => TCNOps.freeNames(obt) ++ TCNOps.freeNames(exp)
  }

  def boundNames(cond: IRCond): Set[String] = cond match {
    case IRCond.And(left, right) => boundNames(left) ++ boundNames(right)
    case IRCond.TypeDecl(pat, _, _) => TCNOps.boundNames(pat) 
    case _ => Set()
  }

  def rename(node: IRNode, currName: String, newName: String): IRNode = node match {
    case IRNode.Unexpected => node
    case IRNode.And(c +: cs, next) =>
      if boundNames(c).contains(currName) then
        node
      else
        val IRNode.And(newCs, newNext) = rename(IRNode.And(cs, next), currName, newName): @unchecked
        IRNode.And(rename(c, currName, newName) +: newCs, newNext)
    case IRNode.And(_, next) => IRNode.And(Nil, rename(next, currName, newName))
    case IRNode.Or(main, alt) => IRNode.Or(
      rename(main, currName, newName),
      rename(alt, currName, newName)
    )
    case IRNode.Error(err) => IRNode.Error(rename(err, currName, newName))
    case IRNode.Switch(bs, o) => IRNode.Switch(
      bs.map({ case (c, b) => (rename(c, currName, newName), rename(b, currName, newName)) }),
      rename(o, currName, newName)
    )
    case IRNode.Type(typ) => IRNode.Type(rename(typ, currName, newName))
    case IRNode.Range(colVar, startIdx, seed, cursor, body) =>
      val newColVar = if colVar == currName then newName else colVar
      val newBody = if cursor == currName || cursor == newName then body else rename(body, currName, newName)
      val newSeed = TCNOps.replace(seed, currName, TCN.Var(newName))
      IRNode.Range(newColVar, startIdx, newSeed, cursor, newBody)
  }

  def rename(cond: IRCond, currName: String, newName: String): IRCond = cond match {
    case IRCond.And(left, right) => IRCond.And(rename(left, currName, newName), rename(right, currName, newName))
    case IRCond.EnvSizeIs(envVar, size) =>
      if envVar == currName then
        IRCond.EnvSizeIs(newName, size)
      else
        cond
    case IRCond.OfType(termCode, typRef) => IRCond.OfType(
      TCNOps.replace(termCode, currName, TCN.Var(newName)),
      typRef
    )
    case IRCond.TermEquals(t1Code, t2Code) => IRCond.TermEquals(
      TCNOps.replace(t1Code, currName, TCN.Var(newName)),
      TCNOps.replace(t2Code, currName, TCN.Var(newName))
    )
    case IRCond.TypeEquals(t1Code, t2Code) => IRCond.TypeEquals(
      TCNOps.replace(t1Code, currName, TCN.Var(newName)),
      TCNOps.replace(t2Code, currName, TCN.Var(newName))
    )
    case IRCond.TypeDecl(declPat, typExp, expect) =>
      if TCNOps.boundNames(declPat).contains(currName) then
        cond
      else
        IRCond.TypeDecl(declPat, rename(typExp, currName, newName), expect.map(rename(_, currName, newName)))
  }

  def rename(typExp: IRType, currName: String, newName: String): IRType = typExp match {
    case IRType.EnvGet(envVar, keyCode) => IRType.EnvGet(
      if envVar == currName then newName else envVar,
      TCNOps.replace(keyCode, currName, TCN.Var(newName))
    )
    case IRType.FromCode(typCode, isOptional) => IRType.FromCode(
      TCNOps.replace(typCode, currName, TCN.Var(newName)),
      isOptional
    )
    case IRType.Induction(expCode, envCode) => IRType.Induction(
      TCNOps.replace(expCode, currName, TCN.Var(newName)),
      TCNOps.replace(envCode, currName, TCN.Var(newName))
    )
  }

  def rename(expect: IRTypeExpect, currName: String, newName: String): IRTypeExpect = expect match {
    case IRTypeExpect.EqualsTo(typCode) => IRTypeExpect.EqualsTo(
      TCNOps.replace(typCode, currName, TCN.Var(newName))
    )
    case IRTypeExpect.OfType(_) => expect 
  }

  def rename(err: IRError, currName: String, newName: String): IRError = err match {
    case IRError.AllOf(errors*) => IRError.AllOf(errors.map(rename(_, currName, newName))*)
    case IRError.OneOf(errors*) => IRError.OneOf(errors.map(rename(_, currName, newName))*) 
    case IRError.Generic(message) => IRError.Generic(TCNOps.replace(message, currName, TCN.Var(newName)))
    case IRError.NoType(exp) => IRError.NoType(TCNOps.replace(exp, currName, TCN.Var(newName)))
    case IRError.UnexpectedEnvSize(env, size) => IRError.UnexpectedEnvSize(
      TCNOps.replace(env, currName, TCN.Var(newName)),
      size
    )
    case IRError.UnexpectedType(obtained, expected) => IRError.UnexpectedType(
      TCNOps.replace(obtained, currName, TCN.Var(newName)),
      TCNOps.replace(expected, currName, TCN.Var(newName))
    )
  }