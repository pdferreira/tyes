package tyes.model

object TyesLanguageExtensions:
  
  extension (metaBinding: Binding)

    def matches(entry: (String, Type.Named)): Option[(Map[String, String], Map[String, Type.Named])] = 
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

    def substitute(termVarSubst: Map[String, String], typeVarSubst: Map[String, Type.Named]): Binding = metaBinding match {
      case Binding.BindName(name, typ) => Binding.BindName(name, typ.substitute(typeVarSubst))
      case Binding.BindVariable(name, typ) => 
        if termVarSubst.contains(name) then
          Binding.BindName(termVarSubst(name), typ.substitute(typeVarSubst))
        else
          Binding.BindVariable(name, typ.substitute(typeVarSubst))
    }

    def toConcrete: Option[(String, Type.Named)] = metaBinding match {
      case Binding.BindName(name, typ @ Type.Named(_)) => Some(name -> typ)
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
    typeVarSubst: Map[String, Type.Named] = Map(),
    envVarSubst: Map[String, Either[String, Map[String, Type.Named]]] = Map()
  )

  extension (envPart: EnvironmentPart)

    def matches(env: Map[String, Type.Named], remainingEnvVar: Option[String]): Option[EnvironmentMatch] = envPart match {
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
              m.copy(envVarSubst = Map(remEnvVarName -> Right(Map.from(remEntries))))
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

    def toConcrete: Option[Map[String, Type.Named]] = envPart match {
      case EnvironmentPart.Bindings(bindings) => 
        bindings.foldLeft(Option(Map[String, Type.Named]())) { (envOpt, binding) =>
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

    def matches(env: Map[String, Type.Named]): Option[EnvironmentMatch] =
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

    def toConcrete: Option[Map[String, Type.Named]] =
      metaEnv.parts.map(_.toConcrete).foldLeft(Option(Map[String, Type.Named]())) { (prevEnvOpt, currEnvOpt) =>
        prevEnvOpt.zip(currEnvOpt).map(_ ++ _)
      }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = metaEnv.parts.flatMap(_.termVariables).toSet

    def envVariables: Set[String] = metaEnv.parts.collect({ case EnvironmentPart.Variable(name) => name }).toSet

    def types: Set[Type] = metaEnv.parts.flatMap(_.types).toSet

  extension (typ: Type)

    def matches(other: Type.Named): Option[Map[String, Type.Named]] = typ match {
      case t @ Type.Named(_) => 
        if t == other then
          Some(Map())
        else
          None
      case Type.Variable(typVarName) =>
        Some(Map(typVarName -> other))
    }

    def substitute(typeVarEnv: Map[String, Type.Named]): Type = typ match {
      case Type.Named(_) => typ
      case Type.Variable(name) => typeVarEnv.getOrElse(name, typ)
    }

    def variables: Set[String] = typ match {
      case Type.Named(_) => Set()
      case Type.Variable(name) => Set(name)
    }

  extension (judg: Judgement)

    def typeVariables: Set[String] = judg.types.flatMap(_.variables)

    def termVariables: Set[String] = judg.assertion.termVariables ++ judg.env.termVariables

    def types: Set[Type] = judg.assertion.types ++ judg.env.types

  extension (asrt: Assertion)

    def typeVariables: Set[String] = types.flatMap(_.variables)
    
    def termVariables: Set[String] = asrt match {
      case HasType(e, _) => e.variables
    }

    def types: Set[Type] = Set(asrt match {
      case HasType(_, typ) => typ
    })

  extension (tsDecl: TypeSystemDecl)

    def types: Set[Type] =
      (for 
        case RuleDecl(_, prems, concl) <- tsDecl.rules
        j <- concl +: prems
        t <- j.types
      yield t).toSet
