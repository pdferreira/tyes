package tyes.model

object indexes:

  private val INDEX_SEP = "_"

  def extractIndex(rawVarName: String): Option[(String, String)] =
    rawVarName.split(INDEX_SEP, 2) match {
      case Array(varName, idxStr) => Some((varName, idxStr))
      case _ => None
    }

  def extractIntIndex(rawVarName: String): Option[(String, Int)] =
    extractIndex(rawVarName) match {
      case Some((varName, idxStr)) => idxStr.toIntOption.map((varName, _))
      case _ => None
    }

  def indexedVar(varName: String, idxStr: String): String = varName + INDEX_SEP + idxStr

  extension (metaBinding: Binding)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Binding = metaBinding match {
      case Binding.BindName(name, typ) => Binding.BindName(name, typ.replaceIndex(oldIdxStr, newIdxStr))
      case Binding.BindVariable(rawName, typ) =>
        val replacedTyp = typ.replaceIndex(oldIdxStr, newIdxStr)
        extractIndex(rawName) match {
          case Some((name, `oldIdxStr`)) => Binding.BindVariable(indexedVar(name, newIdxStr), replacedTyp)
          case _ => Binding.BindVariable(rawName, replacedTyp)
        }
    }

  extension (envPart: EnvironmentPart)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): EnvironmentPart = envPart match {
      case EnvironmentPart.Bindings(bindings) => EnvironmentPart.Bindings(bindings.map(_.replaceIndex(oldIdxStr, newIdxStr)))
      case EnvironmentPart.Variable(envVarName) => extractIndex(envVarName) match {
        case Some((name, `oldIdxStr`)) => EnvironmentPart.Variable(indexedVar(name, newIdxStr))
        case _ => envPart
      }
    }

  extension (metaEnv: Environment)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Environment =
      Environment(metaEnv.parts.map(_.replaceIndex(oldIdxStr, newIdxStr)))

  extension (asrt: Assertion)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Assertion = asrt match {
      case HasType(term, typ) => HasType(
        term.replaceIndex(oldIdxStr, newIdxStr),
        typ.replaceIndex(oldIdxStr, newIdxStr)
      )
    }

  extension (judg: Judgement)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Judgement = Judgement(
      judg.env.replaceIndex(oldIdxStr, newIdxStr),
      judg.assertion.replaceIndex(oldIdxStr, newIdxStr)
    )
