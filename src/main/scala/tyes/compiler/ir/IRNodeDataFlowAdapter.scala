package tyes.compiler.ir

import tyes.compiler.ir.IRNodeScopeOperations.*
import tyes.compiler.ir.IRNodeVisitor.*
import tyes.compiler.target.TargetCodeForCursor
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.{TargetCodeNodeOperations as TCNOps}

/**
  * Transforms a IRNode in such a way that the it is optimized for the required
  * data flow:
  *   1. order of its declarations matches the order of the data flow
  *   2. unused declarations get muted
  * 
  * In a way it transforms from a purely declarative specification into an operational one.
  * 
  * Assumes that declarations within the same node are unique, otherwise
  * the results are not reliable. For example, an And node can't have two 
  * TypeDecls declaring the same variable.
  */
class IRNodeDataFlowAdapter:

  private type TCP = TargetCodePattern
  private val TCP = TargetCodePattern

  def adapt(irNode: IRNode): IRNode = irNode match {
    case IRNode.And(conds, next) =>
      val allUsedNames = conds.flatMap(c => freeNames(c).toSet).toSet ++ freeNames(next).toSet
      val adaptedConds = conds.map(c => adapt(c, allUsedNames))
      IRNode.And(inDataFlowOrder(adaptedConds), adapt(next))

    case _ => applyToChildren(irNode, adapt)
  }

  def adapt(cond: IRCond, allUsedNames: Set[String]): IRCond = cond match {
    case IRCond.TypeDecl(declPat, typExp, expect) =>
      val mutedPat = muteUnused(declPat, allUsedNames) 
      applyToChildren(IRCond.TypeDecl(mutedPat, typExp, expect), adapt)
    case _ => applyToChildren(cond, adapt)
  }

  private def muteUnused(pat: TCP, allUsedNames: Set[String]): TCP = pat match {
    case TCP.Var(name) if !allUsedNames.contains(name) => TCP.Any
    case _ => TCNOps.applyToChildren(pat, muteUnused(_, allUsedNames))
  }

  /**
    * Ensures the collection order respects the data flow, taking into account
    * the order of declarations. Preserves the provided relative order between conditions
    * except in when it detects used-before-declared scenarios.
    *
    * @param conds
    * @return Reordered collection of conditions
    */
  private def inDataFlowOrder(conds: Seq[IRCond]): Seq[IRCond] =

    // Build a map of all the undeclared dependencies for each condition
    // but only considering the names that are actually declared within this scope
    // otherwise this is pointless because there'll always be `exp` and other free names
    val undeclaredNames = Set.from(conds.map(boundNames).flatten)
    val missingDepsByCond = conds
      .map { c =>
        val missingDeps = freeNames(c).toSet.intersect(undeclaredNames)
        (c, collection.mutable.Set.from(missingDeps))
      }
      .toMap

    // Go through all the conditions, preferrably in order but picking those with
    // 0 missing dependencies first. For each pick, update the missing dependencies
    // and if there're none with 0 dependencies, fallback to picking the first one.
    val res = collection.mutable.ListBuffer[IRCond]()
    val condsToProcess = collection.mutable.Queue.from(conds)
    while !condsToProcess.isEmpty do
      val c = condsToProcess
        .dequeueFirst(c => missingDepsByCond(c).isEmpty)
        .getOrElse { condsToProcess.dequeue() }
      
      for 
        name <- boundNames(c)
        deps <- missingDepsByCond.values
      do
        deps.remove(name)

      res += c

    // Finally pack the collected conditions
    res.toSeq
