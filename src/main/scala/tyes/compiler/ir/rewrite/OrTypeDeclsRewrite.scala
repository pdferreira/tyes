package tyes.compiler.ir.rewrite

import tyes.compiler.NameOperations
import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRNodeScopeOperations.*
import tyes.compiler.ir.IRType
import tyes.compiler.ir.IRTypeExpect
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.{TargetCodeNodeOperations as TCNOps}

object OrTypeDeclsRewrite extends Rewrite[IRNode]:

  private type TCP = TargetCodePattern
  private val TCP = TargetCodePattern
  private val TCN = TargetCodeNode

  override val tryRewrite = {
    case IRNode.Or(
      IRNode.And(
        IRCond.TypeDecl(p1, e1, ex1) +: cs1,
        n1
      ),
      IRNode.And(
        IRCond.TypeDecl(p2, e2, ex2) +: cs2,
        n2
      )
    ) if e1 == e2 
    =>
      for p <- pickMostSpecific(p1, p2)
      yield
        val allNames = (cs1 ++ cs2).map(freeNames).map(_.toSet).flatten.toSet
        val declVarPat = p match {
          case TCP.Any => 
            val proposedName = getProposedDeclVarName(e1)
            TCP.Var(NameOperations.nameclash(proposedName, allNames))
          case p => p
        }
        val ex = if ex1 == ex2 then ex1 else None
        val declVar = extractVarFromPattern(declVarPat)
        var innerOrDeclName = NameOperations.nameclash("resT", allNames ++ TCNOps.boundNames(declVarPat))
        IRNode.And(
          IRCond.TypeDecl(declVarPat, e1, ex) ::
            IRCond.TypeDecl(
              TCP.Var(innerOrDeclName),
              IRNode.Or(
                extendAndWithExpectation(
                  thisAndNode = IRNode.And(cs1, n1),
                  thisDeclPat = p1,
                  thisExpectation = ex1,
                  otherDeclPat = p2,
                  commonDeclPat = p,
                  commonDeclVar = declVar,
                  commonExpectation = ex
                ),
                extendAndWithExpectation(
                  thisAndNode = IRNode.And(cs2, n2),
                  thisDeclPat = p2,
                  thisExpectation = ex2,
                  otherDeclPat = p1,
                  commonDeclPat = p,
                  commonDeclVar = declVar,
                  commonExpectation = ex
                )),
              None
            ) :: Nil,
          IRNode.Type(IRType.FromCode(TCN.Var(innerOrDeclName), isOptional = false))
        )
  }
    
  private def pickMostSpecific(pat1: TCP, pat2: TCP): Option[TCP] = (pat1, pat2) match {
    case _ if pat1 == pat2 => Some(pat1)
    case (TCP.Any, _) => Some(pat2)
    case (_, TCP.Any) => Some(pat1)
    case (TCP.Var(_), TCP.Var(_)) => Some(pat1) // TODO: choose based on the one not already used in follow-up conditions
    case _ => None
  }

  private def extractVarFromPattern(pat: TCP): TargetCodeNode.Var = pat match {
    case TCP.Var(name) => TCN.Var(name)
    case _ => ???
  }

  private def expectToCond(typeCode: TargetCodeNode, typeExpect: IRTypeExpect): IRCond = typeExpect match {
    case IRTypeExpect.EqualsTo(expectedTypeCode) => IRCond.TypeEquals(typeCode, expectedTypeCode)
    case IRTypeExpect.OfType(typRef) => IRCond.OfType(typeCode, typRef)
  }

  private def getProposedDeclVarName(declExp: IRNode): String = declExp match {
    case IRNode.Type(typ) => typ match {
      case IRType.Induction(TCN.Var(name), _) => NameOperations.getDigitSuffix(name) match {
        case Some(n) => s"t$n"
        case None => "t1"
      }
      case _ => "t"
    }
    case _ => "res"
  }

  private def extendAndWithExpectation(
    thisAndNode: IRNode.And,
    thisDeclPat: TCP,
    thisExpectation: Option[IRTypeExpect],
    otherDeclPat: TCP,
    commonDeclPat: TCP,
    commonDeclVar: TCN.Var,
    commonExpectation: Option[IRTypeExpect]
  ): IRNode.And =
    val IRNode.And(cs, n) = thisDeclPat match {
      case TCP.Var(thisDeclVarName) 
        if commonDeclPat == otherDeclPat
        && otherDeclPat != thisDeclPat
      =>
        rename(thisAndNode, thisDeclVarName, commonDeclVar.name)
      case _ =>
        thisAndNode
    }: @unchecked
    
    IRNode.And(
      if thisExpectation.isEmpty || thisExpectation == commonExpectation then 
        cs
      else
        expectToCond(commonDeclVar, thisExpectation.get) +: cs
      ,
      n
    )
              