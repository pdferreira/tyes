package tyes.compiler.ir

import scala.annotation.tailrec

object IRNodeVisitor:

  @tailrec
  def fixpoint(irNode: IRNode, pf: PartialFunction[IRNode, IRNode]): IRNode =
    val newIRNode = applyUntil(irNode, pf)
    if irNode != newIRNode then
      fixpoint(newIRNode, pf)
    else
      newIRNode

  def applyUntil(irNode: IRNode, pf: PartialFunction[IRNode, IRNode]): IRNode =
    pf.applyOrElse(irNode, applyToChildren(_, c => applyUntil(c, pf)))

  def applyToChildren(irNode: IRNode, f: IRNode => IRNode): IRNode = irNode match {
    case IRNode.Unexpected => irNode
    case IRNode.Error(_) => irNode
    case IRNode.Type(irType) => irNode
    
    case IRNode.And(Seq(), next) => IRNode.And(Seq(), f(next))

    case IRNode.And(c +: cs, next) =>
      val newC = applyToChildren(c, f)

      f(IRNode.And(cs, next)) match {
        case IRNode.And(newCs, newNext) => IRNode.And(newC +: newCs, newNext)
        case _ => ???
      }
    
    case IRNode.Or(main, alt) =>
      IRNode.Or(f(main), f(alt))
    
    case IRNode.Switch(bs, o) =>
      IRNode.Switch(
        branches = for (c, b) <- bs yield (c, f(b)),
        otherwise = f(o)
      )
  }

  def applyToChildren(irCond: IRCond, f: IRNode => IRNode): IRCond = irCond match {
    case IRCond.EnvSizeIs(_, _) => irCond
    case IRCond.TypeEquals(_, _) => irCond
    case IRCond.TypeDecl(declPat, typExp, expect) => IRCond.TypeDecl(declPat, f(typExp), expect)
  }
