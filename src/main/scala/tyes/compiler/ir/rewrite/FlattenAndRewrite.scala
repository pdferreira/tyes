package tyes.compiler.ir.rewrite

import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRNodeScopeOperations.*

object FlattenAndRewrite extends Rewrite[IRNode]:

  override val tryRewrite = {
    case IRNode.And(
      IRCond.TypeDecl(
        p1,
        IRNode.And(c2 +: (cs2 @ _ +: _), n2),
        ex1
      ) +: cs1,
      n1
    ) =>
      // TODO: if boundNames(c2) are in freeNames(cs1 && n1)
      // it needs to be renamed. For now we just assert that's not the case
      assert((boundNames(c2) & freeNames(IRNode.And(cs1, n1)).toSet).isEmpty)
      
      val flattenedNode = IRNode.And(
        c2 +: IRCond.TypeDecl(p1, IRNode.And(cs2, n2), ex1) +: cs1,
        n1
      )
      Some(flattenedNode)
  }
