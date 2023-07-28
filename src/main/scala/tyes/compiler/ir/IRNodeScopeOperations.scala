package tyes.compiler.ir

import utils.collections.Multiset
import tyes.compiler.target.{TargetCodeNodeOperations as TCNOps}

object IRNodeScopeOperations:

  def freeNames(irNode: IRNode): Multiset[String] = irNode match {
    case IRNode.Unexpected => Multiset()
    case IRNode.Error(err) => freeNames(err)
    case IRNode.Type(typExp) => freeNames(typExp)
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