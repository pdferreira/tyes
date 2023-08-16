package tyes.compiler.ir

import IRNodeScopeOperations.*
import IRNodeVisitor.*
import tyes.compiler.RuntimeAPIGenerator
import tyes.compiler.ir.rewrite.*
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.{TargetCodeNodeOperations as TCNOps}
import utils.collections.*

class IRNodeSimplifier:

  private type TCP = TargetCodePattern
  private val TCP = TargetCodePattern
  private val TCN = TargetCodeNode

  def simplify(irNode: IRNode): IRNode = fixpoint(irNode, {
    // Flatten And(Decl(And), ...) 
    case origNode @ FlattenAndRewrite(simplifiedNode) => 
      debug("Flattening in", origNode)
      simplifiedNode

    // Extract Or(And, And) to And(..., Or, ...)
    case origNode @ OrTypeDeclsRewrite(simplifiedNode) => 
      debug("Extracting Or to And in", origNode)
      simplifiedNode

    // Give precendence to Induction Decls over the remaining conds
    case origNode @ IRNode.And(
      c1 +: (c2 @ IRCond.TypeDecl(p, IRNode.Type(_: IRType.Induction), _)) +: cs,
      n
    ) 
      if (c1 match { case IRCond.TypeDecl(_, IRNode.Type(_: IRType.Induction), _) => false ; case _ => true })
      && (freeNames(c1).toSet intersect TCNOps.boundNames(p)).isEmpty
      && (freeNames(c2).toSet intersect boundNames(c1)).isEmpty
      =>
        debug("Promoting induction decl in", origNode)
        IRNode.And(c2 +: c1 +: cs, n)

    // Aggregate non-Induction conditions as a switch 
    case origNode @ NonInductionCondsToSwitchRewrite(simplifiedNode) =>
        debug("Aggregating non-Induction conds in", origNode)
        simplifiedNode

    // Merge non-overlapping Switches
    case origNode @ MergeNonOverlappingSwitchesRewrite(simplifiedNode) =>
        debug("Merging non-overlapping Switches in", origNode)
        simplifiedNode
  })

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
