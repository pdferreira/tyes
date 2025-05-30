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

  def extractRangeVariable(from: Judgement, to: Judgement): (String, Range) =
    val HasType(Term.Variable(fromVar), _) = from.assertion: @unchecked
    val HasType(Term.Variable(toVar), _) = to.assertion: @unchecked

    val Some((fromIdent, fromIdx)) = extractIntIndex(fromVar): @unchecked
    val Some((toIdent, toIdxStr)) = extractIndex(toVar): @unchecked

    val toIdx = toIdxStr.toIntOption.getOrElse(Int.MaxValue)
    (fromIdent, Range.inclusive(fromIdx, toIdx))

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

  extension (term: Term)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Term = term match {
      case Term.Constant(_) => term
      case Term.Variable(rawName) => extractIndex(rawName) match {
        case Some((name, `oldIdxStr`)) => Term.Variable(indexedVar(name, newIdxStr))
        case _ => term
      }
      case Term.Function(name, args*) => Term.Function(name, args.map(_.replaceIndex(oldIdxStr, newIdxStr))*)
      case Term.Type(typ) => Term.Type(typ.replaceIndex(oldIdxStr, newIdxStr))
    }

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

  extension (typ: Type)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Type = typ match {
      case Type.Named(name) => typ
      case Type.Variable(rawName) => extractIndex(rawName) match {
        case Some((name, `oldIdxStr`)) => Type.Variable(indexedVar(name, newIdxStr))
        case _ => Type.Variable(rawName)
      }
      case Type.Composite(name, args*) => Type.Composite(name, args.map(_.replaceIndex(oldIdxStr, newIdxStr))*)
    }

