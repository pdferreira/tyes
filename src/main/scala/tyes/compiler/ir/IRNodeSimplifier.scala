package tyes.compiler.ir

import IRNodeVisitor.*
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeNode

class IRNodeSimplifier:

  private val TCP = TargetCodePattern
  private val TCN = TargetCodeNode

  def simplify(irNode: IRNode): IRNode = fixpoint(irNode, {
    case IRNode.And(
      IRCond.TypeDecl(
        p1,
        IRNode.And(c2 +: (cs2 @ _ +: _), n2),
        ex1
      ) +: cs1,
      n1
    ) => 
      // missing: if boundNames(c2) are in freeNames(cs1 && n1)
      // it needs to be renamed
      IRNode.And(
        c2 +: IRCond.TypeDecl(p1, IRNode.And(cs2, n2), ex1) +: cs1,
        n1
      )

    case IRNode.Or(
      IRNode.And(
        IRCond.TypeDecl(p1, e1, ex1) +: cs1,
        n1
      ),
      IRNode.And(
        IRCond.TypeDecl(p2, e2, ex2) +: cs2,
        n2
      )
    ) if e1 == e2 && p1 == p2 && ex1 == ex2 => 
      IRNode.And(
        IRCond.TypeDecl(p1, e1, ex1) ::
          IRCond.TypeDecl(
            TCP.Var("resT"),
            IRNode.Or(
              IRNode.And(cs1, n1),
              IRNode.And(cs2, n2)
            ),
            None
          ) :: Nil,
        IRNode.Type(IRType.FromCode(TCN.Var("resT"), isOptional = false))
      )
  })
