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

    def typeVariables: Set[String] = metaEnv match {
      case Environment.BindName(_, typ) => typ.variables
    }

  extension (typ: Type)

    def substitute(typeVarEnv: Map[String, Type.Named]): Type = typ match {
      case Type.Named(_) => typ
      case Type.Variable(name) => typeVarEnv(name)
    }

    def variables: Set[String] = typ match {
      case Type.Named(_) => Set()
      case Type.Variable(name) => Set(name)
    }
