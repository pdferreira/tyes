package tyes.model

import tyes.model.indexes.*
import tyes.model.ranges.*
import tyes.model.TyesLanguageExtensions.*

object scope:

  extension (premise: Premise)

    def bindsTypeVariableInEnv(varName: String): Boolean = premise match {
      case Judgement(env, _) => env.typeVariables.contains(varName)
      case JudgementRange(from, to) =>
        val (_, fromIdx, toIdx) = extractRangeVariable(from, to)
        extractIntIndex(varName) match {
          case Some((rootVarName, idx)) =>
            fromIdx <= idx && idx <= toIdx.asNumber.map(_.value).getOrElse(Int.MaxValue)
            && from.bindsTypeVariableInEnv(indexedVar(rootVarName, fromIdx.toString))
          
          case _ => from.bindsTypeVariableInEnv(varName) || to.bindsTypeVariableInEnv(varName)
        }
    }

    def bindsTypeVariableInTerm(varName: String): Boolean = premise match {
      case Judgement(_, term) => term.typeVariables.contains(varName)
      case JudgementRange(from, to) =>
        val (_, fromIdx, toIdx) = extractRangeVariable(from, to)
        extractIntIndex(varName) match {
          case Some((rootVarName, idx)) =>
            fromIdx <= idx && idx <= toIdx.asNumber.map(_.value).getOrElse(Int.MaxValue)
            && from.bindsTypeVariableInTerm(indexedVar(rootVarName, fromIdx.toString))
          case _ => from.bindsTypeVariableInTerm(varName) || to.bindsTypeVariableInTerm(varName)
        }
    }