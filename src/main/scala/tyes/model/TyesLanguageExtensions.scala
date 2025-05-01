package tyes.model

import tyes.model.indexes.*

object TyesLanguageExtensions:
  
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

    def matches(entry: (String, Type)): Option[(Map[String, String], Map[String, Type])] = 
      val (entryName, entryTyp) = entry
      metaBinding match {
        case Binding.BindName(name, typ) =>
          if entryName == name then
            typ.matches(entryTyp).map((Map(), _))
          else
            None
        case Binding.BindVariable(name, typ) =>
          typ.matches(entryTyp).map((Map(name -> entryName), _))
      }

    def substitute(termVarSubst: Map[String, String], typeVarSubst: Map[String, Type]): Binding = metaBinding match {
      case Binding.BindName(name, typ) => Binding.BindName(name, typ.substitute(typeVarSubst))
      case Binding.BindVariable(name, typ) => 
        if termVarSubst.contains(name) then
          Binding.BindName(termVarSubst(name), typ.substitute(typeVarSubst))
        else
          Binding.BindVariable(name, typ.substitute(typeVarSubst))
    }

    def toConcrete: Option[(String, Type)] = metaBinding match {
      case Binding.BindName(name, typ) if typ.isGround => Some(name -> typ)
      case _ => None
    }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = metaBinding match {
      case Binding.BindName(_, _) => Set()
      case Binding.BindVariable(name, _) => Set(name)
    }

    def types: Set[Type] = metaBinding match {
      case Binding.BindName(_, typ) => Set(typ)
      case Binding.BindVariable(_, typ) => Set(typ)
    }

  case class EnvironmentMatch(
    termVarSubst: Map[String, String] = Map(),
    typeVarSubst: Map[String, Type] = Map(),
    envVarSubst: Map[String, Either[String, Map[String, Type]]] = Map()
  )

  extension (envPart: EnvironmentPart)

    def matches(env: Map[String, Type], remainingEnvVar: Option[String]): Option[EnvironmentMatch] = envPart match {
      case EnvironmentPart.Bindings(bindings) =>
        if bindings.isEmpty != env.isEmpty then
          None
        else
          val (remEntries, matchRes) = bindings.foldLeft((env.toSeq, Option(EnvironmentMatch()))) { case ((entries, prevEnvMatchOpt), b) =>
            prevEnvMatchOpt.map { prevEnvMatch =>
              val matches = for e <- entries ; m <- b.matches(e) yield (e, m)
              if matches.isEmpty then
                (Seq(), Option.empty)
              else
                val (entry, (termVarSubst, typeVarSubst)) = matches.head
                val newEnvMatch = EnvironmentMatch(
                  prevEnvMatch.termVarSubst ++ termVarSubst,
                  prevEnvMatch.typeVarSubst ++ typeVarSubst
                )
                (entries.filterNot(_ == entry), Option(newEnvMatch))
            }.getOrElse((Seq(), Option.empty))
          }

          if remEntries.isEmpty then
            matchRes
          else
            remainingEnvVar.zip(matchRes).map { (remEnvVarName, m) => 
              val remEntriesUpdated = remEntries.map((name, typ) => name -> typ.substitute(m.typeVarSubst))
              m.copy(envVarSubst = Map(remEnvVarName -> Right(Map.from(remEntriesUpdated))))
            }
      case EnvironmentPart.Variable(envVarName) => Some(EnvironmentMatch(envVarSubst = Map(envVarName -> Right(env))))
    }

    def substitute(envMatch: EnvironmentMatch): EnvironmentPart = envPart match {
      case EnvironmentPart.Bindings(bindings) => EnvironmentPart.Bindings(bindings.map(_.substitute(envMatch.termVarSubst, envMatch.typeVarSubst)))
      case EnvironmentPart.Variable(envVarName) =>
        envMatch.envVarSubst.get(envVarName) match {
          case None => envPart
          case Some(Left(newEnvVarName)) => EnvironmentPart.Variable(newEnvVarName)
          case Some(Right(env)) => EnvironmentPart.Bindings(env.toSeq.map((name, typ) => Binding.BindName(name, typ)))
        } 
    }

    def replaceIndex(oldIdxStr: String, newIdxStr: String): EnvironmentPart = envPart match {
      case EnvironmentPart.Bindings(bindings) => EnvironmentPart.Bindings(bindings.map(_.replaceIndex(oldIdxStr, newIdxStr)))
      case EnvironmentPart.Variable(envVarName) => extractIndex(envVarName) match {
        case Some((name, `oldIdxStr`)) => EnvironmentPart.Variable(indexedVar(name, newIdxStr))
        case _ => envPart
      }
    }

    def toConcrete: Option[Map[String, Type]] = envPart match {
      case EnvironmentPart.Bindings(bindings) => 
        bindings.foldLeft(Option(Map[String, Type]())) { (envOpt, binding) =>
          binding.toConcrete.zip(envOpt).map { (entry, env) => env + entry }
        }
      case EnvironmentPart.Variable(_) => Some(Map())
    }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = envPart match {
      case EnvironmentPart.Bindings(bindings) => bindings.flatMap(_.termVariables).toSet
      case EnvironmentPart.Variable(_) => Set()
    }

    def types: Set[Type] = envPart match {
      case EnvironmentPart.Bindings(bindings) => bindings.flatMap(_.types).toSet
      case EnvironmentPart.Variable(_) => Set()
    }

  extension (metaEnv: Environment)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Environment =
      Environment(metaEnv.parts.map(_.replaceIndex(oldIdxStr, newIdxStr)))

    def matches(env: Map[String, Type]): Option[EnvironmentMatch] =
      val remainingEnvVars = metaEnv.envVariables
      if remainingEnvVars.size > 1 then
        throw new NotImplementedError("Environments with more than one environment variable are unsupported")
      
      val allBindingSeqs = metaEnv.parts.collect({ case EnvironmentPart.Bindings(bs) => bs })
      if allBindingSeqs.isEmpty then
        remainingEnvVars.headOption.map(envVarName => 
          EnvironmentMatch(envVarSubst = Map(envVarName -> Right(env)))
        )
      else
        EnvironmentPart.Bindings(allBindingSeqs.flatten).matches(env, remainingEnvVars.headOption)

    def substitute(envMatch: EnvironmentMatch): Environment =
      Environment(metaEnv.parts.map(_.substitute(envMatch)))

    def toConcrete: Option[Map[String, Type]] =
      metaEnv.parts.map(_.toConcrete).foldLeft(Option(Map[String, Type]())) { (prevEnvOpt, currEnvOpt) =>
        prevEnvOpt.zip(currEnvOpt).map(_ ++ _)
      }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = metaEnv.parts.flatMap(_.termVariables).toSet

    def envVariables: Set[String] = metaEnv.parts.collect({ case EnvironmentPart.Variable(name) => name }).toSet

    def types: Set[Type] = metaEnv.parts.flatMap(_.types).toSet

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

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Iterable[Term.Variable | Type.Variable] = term match {
      case Term.Constant(_) => Set.empty
      case v: Term.Variable => Set(v)
      case Term.Function(_, args*) => args.flatMap(_.termVariables).toSet
      case Term.Type(typ) => typ.typeVariables
    }

    def types: Set[Type] = term match {
      case Term.Constant(_) => Set()
      case Term.Variable(_) => Set()
      case Term.Function(_, args*) => args.flatMap(_.types).toSet
      case Term.Type(t) => Set(t)
    }

  extension (asrt: Assertion)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Assertion = asrt match {
      case HasType(term, typ) => HasType(
        term.replaceIndex(oldIdxStr, newIdxStr),
        typ.replaceIndex(oldIdxStr, newIdxStr)
      )
    }

    def typeVariables: Set[String] = types.flatMap(_.variables)
    
    def termVariables: Set[String] = asrt match {
      case HasType(e, _) => e.variables
    }

    def types: Set[Type] = Set(asrt match {
      case HasType(term, typ) => typ
    })

  extension (judg: Judgement)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Judgement = Judgement(
      judg.env.replaceIndex(oldIdxStr, newIdxStr),
      judg.assertion.replaceIndex(oldIdxStr, newIdxStr)
    )

  extension (prem: Premise)

    def typeVariables: Set[String] = prem match {
      case judg: Judgement => judg.types.flatMap(_.variables)
      case JudgementRange(from, to) => from.typeVariables ++ to.typeVariables
    }

    def termVariables: Set[String] = prem match {
      case judg: Judgement => judg.assertion.termVariables ++ judg.env.termVariables
      case JudgementRange(from, to) => from.termVariables ++ to.termVariables
    }

    def types: Set[Type] = prem match {
      case judg: Judgement => judg.assertion.types ++ judg.env.types
      case JudgementRange(from, to) => from.types ++ to.types
    }

  extension (ruleDecl: RuleDecl)

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = ruleDecl.conclusion.termVariables ++ ruleDecl.premises.flatMap(_.termVariables)

    def types: Set[Type] = ruleDecl.conclusion.types ++ ruleDecl.premises.flatMap(_.types)

  extension (tsDecl: TypeSystemDecl)

    def types: Set[Type] =
      (for 
        case RuleDecl(_, prems, concl) <- tsDecl.rules
        j <- concl +: prems
        t <- j.types
      yield t).toSet
  
  extension (typ: Type)

    def replaceIndex(oldIdxStr: String, newIdxStr: String): Type = typ match {
      case Type.Named(name) => typ
      case Type.Variable(rawName) => extractIndex(rawName) match {
        case Some((name, `oldIdxStr`)) => Type.Variable(indexedVar(name, newIdxStr))
        case _ => Type.Variable(rawName)
      }
      case Type.Composite(name, args*) => Type.Composite(name, args.map(_.replaceIndex(oldIdxStr, newIdxStr))*)
    }

    def typeVariables: Iterable[Type.Variable] = typ match {
      case Type.Named(_) => Set.empty
      case v: Type.Variable => Set(v)
      case Type.Composite(_, args*) => args.flatMap(_.typeVariables).toSet
    }
