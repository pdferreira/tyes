package tyes.model

import tyes.model.indexes.*
import tyes.model.TyesLanguageExtensions.*

object scope:

  extension (premise: Premise)

    def bindsTypeVariableInEnv(varName: String): Boolean = premise match {
      case Judgement(env, _) => env.typeVariables.contains(varName)
      case JudgementRange(from, to) =>
        val (_, idxRange) = extractRangeVariable(from, to)
        extractIntIndex(varName) match {
          case Some((rootVarName, idx)) =>
            idxRange.contains(idx)
            && from.bindsTypeVariableInEnv(indexedVar(rootVarName, idxRange.start.toString))
          
          case _ => from.bindsTypeVariableInEnv(varName) || to.bindsTypeVariableInEnv(varName)
        }
    }

    def bindsTypeVariableInTerm(varName: String): Boolean = premise match {
      case Judgement(_, term) => term.typeVariables.contains(varName)
      case JudgementRange(from, to) =>
        val (_, idxRange) = extractRangeVariable(from, to)
        extractIntIndex(varName) match {
          case Some((rootVarName, idx)) =>
            idxRange.contains(idx)
            && from.bindsTypeVariableInTerm(indexedVar(rootVarName, idxRange.start.toString))
          case _ => from.bindsTypeVariableInTerm(varName) || to.bindsTypeVariableInTerm(varName)
        }
    }