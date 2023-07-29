package tyes.compiler.ir.rewrite

import tyes.compiler.NameOperations
import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRNodeScopeOperations.*
import tyes.compiler.ir.IRType
import tyes.compiler.ir.IRTypeExpect
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern

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
        IRNode.And(
          IRCond.TypeDecl(declVarPat, e1, ex) ::
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
  }
    
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

  private def extractVarFromPattern(pat: TCP): TargetCodeNode = pat match {
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
