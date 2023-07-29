package tyes.compiler.ir.rewrite

import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRCond

object MergeNonOverlappingSwitchesRewrite extends Rewrite[IRNode]:

  override protected val tryRewrite = {
    case IRNode.Or(IRNode.Switch(bs1, IRNode.Error(err1)), IRNode.Switch(bs2, IRNode.Error(err2)))
      if areNonOverlapping(bs1.map(_._1), bs2.map(_._1))
      =>
        val mergedNode = IRNode.Switch(
          branches = bs1 ++ bs2,
          otherwise = IRNode.Error(flattenError(IRError.OneOf(err1, err2)))
        )
        Some(mergedNode)
  }

  
  private def flattenError(err: IRError): IRError = err match {
    case IRError.OneOf(errors*) =>
      val allErrors = errors.flatMap(e => flattenError(e) match {
        case IRError.OneOf(innerErrors*) => innerErrors
        case fe => Seq(fe)
      })
      IRError.OneOf(allErrors*)
    case _ => err
  }

  private def areNonOverlapping(conds1: Iterable[IRCond], conds2: Iterable[IRCond]): Boolean =
    // For every condition in conds1, if it is true, can any condition in conds2 still be true?
    // i.e. (c1_1 || ... || c1_n) => !(c2_1 || ... || c2_m)
    //
    // Follows a simplified version of this verification, which should be enough in practice.
    conds1.forall(c1 => conds2.forall(c2 => impliesFalsehood(c1, c2)))

  private def impliesFalsehood(premise: IRCond, toFalsify: IRCond): Boolean = (premise, toFalsify) match {
    case (IRCond.And(c1, c2), _) => impliesFalsehood(c1, toFalsify) || impliesFalsehood(c2, toFalsify)
    case (_, IRCond.And(c1, c2)) => impliesFalsehood(premise, c1) || impliesFalsehood(premise, c2)
    case (IRCond.EnvSizeIs(v1, s1), IRCond.EnvSizeIs(v2, s2)) => v1 == v2 && s1 != s2
    case (IRCond.TypeEquals(pt1, pt2), IRCond.TypeEquals(ft1, ft2)) =>
      pt1 == ft1 && pt2 != ft2
      || pt1 == ft2 && pt2 != ft1
      || pt2 == ft1 && pt1 != ft2
      || pt2 == ft2 && pt1 != ft1
    case _ => false // TODO: incomplete
  }

