package tyes.compiler.ir

import IRNodeVisitor.*
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeNode
import utils.collections.*
import tyes.compiler.RuntimeAPIGenerator

class IRNodeSimplifier:

  private type TCP = TargetCodePattern
  private val TCP = TargetCodePattern
  private val TCN = TargetCodeNode

  def simplify(irNode: IRNode): IRNode = fixpoint(irNode, {
    // Flatten And(Decl(And), ...) 
    case childNode @ IRNode.And(
      IRCond.TypeDecl(
        p1,
        IRNode.And(c2 +: (cs2 @ _ +: _), n2),
        ex1
      ) +: cs1,
      n1
    ) => 
      debug("Flattening in", childNode)
      // TODO: if boundNames(c2) are in freeNames(cs1 && n1)
      // it needs to be renamed
      IRNode.And(
        c2 +: IRCond.TypeDecl(p1, IRNode.And(cs2, n2), ex1) +: cs1,
        n1
      )

    // Extract Or(And, And) to And(..., Or, ...)
    case childNode @ IRNode.Or(
      IRNode.And(
        IRCond.TypeDecl(p1, e1, ex1) +: cs1,
        n1
      ),
      IRNode.And(
        IRCond.TypeDecl(p2, e2, ex2) +: cs2,
        n2
      )
    ) if 
      e1 == e2 
      && pickMostSpecific(p1, p2).isDefined
    => 
      debug("Extracting Or to And in", childNode)
      val p = pickMostSpecific(p1, p2).get match {
        case TCP.Any => TCP.Var("someT")
        case p => p
      }
      val ex = if ex1 == ex2 then ex1 else None
      val declVar = extractVarFromPattern(p)
      IRNode.And(
        IRCond.TypeDecl(p, e1, ex) ::
          IRCond.TypeDecl(
            TCP.Var("resT"),
            IRNode.Or(
              IRNode.And(
                if ex1.isEmpty || ex1 == ex then 
                  cs1
                else
                  expectToCond(declVar, ex1.get) +: cs1,
                n1
              ),
              IRNode.And(
                if ex2.isEmpty || ex2 == ex then 
                  cs2
                else
                  expectToCond(declVar, ex2.get) +: cs2,
                n2
              )
            ),
            None
          ) :: Nil,
        IRNode.Type(IRType.FromCode(TCN.Var("resT"), isOptional = false))
      )

    // Give precendence to Induction Decls over the remaining conds
    case childNode @ IRNode.And(
      c1 +: (c2 @ IRCond.TypeDecl(_, IRNode.Type(_: IRType.Induction), _)) +: cs,
      n
    ) 
      if !c1.isInstanceOf[IRCond.TypeDecl]
        // && decl var of c2 not in freeNames(c1) 
      =>
        debug("Promoting induction decl in", childNode)
        IRNode.And(c2 +: c1 +: cs, n)

    // Aggregate non-Induction conditions as a switch 
    case childNode @ IRNode.And(cs @ _ +: _, n) 
      if cs.forall(c => c match {
        case IRCond.TypeDecl(_, IRNode.Type(_: IRType.FromCode), Some(_)) => true
        case IRCond.TypeDecl(_, _, _) => false
        case _ => true
      })
      =>
        debug("Aggregating non-Induction conds in", childNode)
        IRNode.Switch(
          branches = Seq(
            cs.foldLeft1(IRCond.And.apply) -> n
          ),
          otherwise =
            val errors = cs.map(condToError)
            if errors.length == 1 then
              IRNode.Error(errors.head)
            else
              IRNode.Error(IRError.AllOf(errors*))
        )

    // Merge non-overlapping Switches
    case childNode @ IRNode.Or(IRNode.Switch(bs1, IRNode.Error(err1)), IRNode.Switch(bs2, IRNode.Error(err2)))
      if areNonOverlapping(bs1.map(_._1), bs2.map(_._1))
      =>
        debug("Merging non-overlapping Switches in", childNode)
        IRNode.Switch(
          branches = bs1 ++ bs2,
          otherwise = IRNode.Error(flattenError(IRError.OneOf(err1, err2)))
        )

  })

  private def pickMostSpecific(pat1: TCP, pat2: TCP): Option[TCP] = (pat1, pat2) match {
    case _ if pat1 == pat2 => Some(pat1)
    case (TCP.Any, _) => Some(pat2)
    case (_, TCP.Any) => Some(pat1)
    case _ => None
  }

  private def pickMostSpecific[T](opt1: Option[T], opt2: Option[T]): Option[Option[T]] = (opt1, opt2) match {
    case (Some(v1), Some(v2)) if v1 != v2 => None
    case _ => Some(opt1.orElse(opt2))
  }

  private def expectToCond(typeCode: TargetCodeNode, typeExpect: IRTypeExpect): IRCond = typeExpect match {
    case IRTypeExpect.EqualsTo(expectedTypeCode) => IRCond.TypeEquals(typeCode, expectedTypeCode)
    case IRTypeExpect.OfType(typRef) => IRCond.OfType(typeCode, typRef)
  }

  private def extractVarFromPattern(pat: TCP): TargetCodeNode = pat match {
    case TCP.Var(name) => TCN.Var(name)
    case _ => ???
  }

  private def condToError(cond: IRCond): IRError = cond match {
    case IRCond.EnvSizeIs(envVar, size) => IRError.UnexpectedEnvSize(TCN.Var(envVar), size)
    case IRCond.TypeEquals(t1Code, t2Code) => IRError.UnexpectedType(t1Code, t2Code)
    case IRCond.TypeDecl(_, IRNode.Type(irTyp), Some(IRTypeExpect.EqualsTo(expectedTypeCode))) =>
      val typCode = irTyp match {
        case IRType.FromCode(typCode, false) => typCode
      }
      IRError.UnexpectedType(typCode, expectedTypeCode)
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

  private def debug(ctx: String, n: IRNode): Unit =
    // println(s"$ctx\r\n\t${toCodeStr(n)}\r\n")
    ()


def toCodeStr(n: IRNode) = new tyes.compiler.target.ScalaTargetCodeGenerator().generate(new TargetCodeIRGeneratorImpl(TCN.Var("exp")).generate(n))

@main def debug =
  import tyes.compiler.ir.*
  import tyes.compiler.target.*
  val commonInstr = IRCond.TypeDecl(
    TargetCodePattern.Var("t1"),
    IRNode.Type(IRType.Induction(TCN.Var("e1"), TCN.Var("env"))),
    None
  )
  val diffDecl1 = IRCond.TypeDecl(
    TargetCodePattern.Var("t2"),
    IRNode.Type(IRType.Induction(TCN.Var("e2"), TCN.Var("env"))),
    None
  )
  val diffDecl2 = IRCond.TypeDecl(
    TargetCodePattern.Any,
    IRNode.Type(IRType.Induction(TCN.Var("e2"), TCN.Var("env"))),
    Some(IRTypeExpect.EqualsTo(TCN.ADTConstructorCall(TCTypeRef("Type", "Xpto"))))
  )
  val n = IRNode.Or(
    IRNode.And(
      commonInstr :: diffDecl1 :: IRCond.EnvSizeIs("env", 1) :: Nil,
      IRNode.Type(IRType.FromCode(TCN.Var("t1"), isOptional = false))
    ), 
    IRNode.And(
      commonInstr :: diffDecl2 :: IRCond.EnvSizeIs("env", 2) :: Nil, 
      IRNode.Type(IRType.FromCode(TCN.Var("z"), isOptional = false))
    )
  )
  
  val simpled = new IRNodeSimplifier().simplify(n)
  println("[Before]")
  println(toCodeStr(n))
  println()

  println("[After]")
  println(toCodeStr(simpled))
