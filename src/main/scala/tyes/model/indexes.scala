package tyes.model

object indexes:

  private val INDEX_SEP = "_"

  def extractIndex(rawVarName: String): Option[(String, String)] =
    rawVarName.split(INDEX_SEP, 2) match {
      case Array(varName, idxStr) => Some((varName, idxStr))
      case _ => None
    }

  def indexedVar(varName: String, idxStr: String): String = varName + INDEX_SEP + idxStr
  
