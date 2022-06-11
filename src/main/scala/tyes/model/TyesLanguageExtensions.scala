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

    def types: Set[Type] = Set(metaBinding match {
      case Binding.BindName(_, typ) => typ
      case Binding.BindVariable(_, typ) => typ
    })

  extension (metaEnv: Environment)

    def matches(env: Map[String, Type.Named]): Option[(Map[String, String], Map[String, Type.Named])] =
      if metaEnv.bindings.isEmpty != env.isEmpty then
        None
      else
        val (remEntries, matchRes) = metaEnv.bindings.foldLeft((env.toSeq, Option((Map[String, String](), Map[String, Type.Named]())))) { case ((entries, substsOpt), b) =>
          substsOpt.map { (prevTermVarSubst, prevTypeVarSubst) =>
            val matches = for e <- entries ; m <- b.matches(e) yield (e, m)
            if matches.isEmpty then
              (Seq(), Option.empty)
            else
              val (entry, (termVarSubst, typeVarSubst)) = matches.head
              (entries.filterNot(_ == entry), Option((prevTermVarSubst ++ termVarSubst, prevTypeVarSubst ++ typeVarSubst)))
          }.getOrElse((Seq(), Option.empty))
        }
        if remEntries.isEmpty then
          matchRes
        else
          None

    def substitute(termVarSubst: Map[String, String], typeVarSubst: Map[String, Type.Named]): Environment = 
      Environment(metaEnv.bindings.map(_.substitute(termVarSubst, typeVarSubst)))

    def toConcrete: Option[Map[String, Type.Named]] = 
      metaEnv.bindings.foldLeft(Option(Map[String, Type.Named]())) { (envOpt, binding) =>
        binding.toConcrete.zip(envOpt).map { (entry, env) => env + entry }
      }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = metaEnv.bindings.flatMap(_.termVariables).toSet

    def types: Set[Type] = metaEnv.bindings.flatMap(_.types).toSet

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

    def termVariables: Set[String] = judg.assertion.termVariables ++ judg.env.fold(Set())(_.termVariables)

    def types: Set[Type] = judg.assertion.types ++ judg.env.fold(Set())(_.types)

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
