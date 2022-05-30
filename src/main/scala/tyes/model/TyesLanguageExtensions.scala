package tyes.model

object TyesLanguageExtensions:
  
  extension (metaEnv: Environment)

    def matches(env: Map[String, Type.Named]): Option[Map[String, Type.Named]] = metaEnv match {
      case Environment.BindName(name, typ) =>
        if env.contains(name) && env.size == 1 then
          typ match {
            case t @ Type.Named(_) => 
              if t == env(name) then
                Some(Map())
              else
                None
            case Type.Variable(typVarName) =>
              Some(Map(typVarName -> env(name)))
          }
        else
          None
    }

    def substitute(typeVarEnv: Map[String, Type.Named]): Environment = metaEnv match {
      case Environment.BindName(name, typ) => Environment.BindName(name, typ.substitute(typeVarEnv))
    }

    def toConcrete: Option[Map[String, Type.Named]] = metaEnv match {
      case Environment.BindName(name, typ @ Type.Named(_)) => Some(Map(name -> typ))
      case _ => None
    }

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Set[String] = metaEnv match {
      case Environment.BindName(_, _) => Set()
    }

    def types: Set[Type] = Set(metaEnv match {
      case Environment.BindName(_, typ) => typ
      case Environment.BindVariable(_, typ) => typ
    })

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
      case Type.Variable(name) => typeVarEnv(name)
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
