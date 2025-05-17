package tyes.model

import tyes.model.indexes.*
import tyes.model.ranges.*
import tyes.model.terms.*

object TyesLanguageExtensions:
  
  extension (metaBinding: Binding)

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

    def typeVariables: Set[String] = types.flatMap(_.variables)

    def termVariables: Iterable[Term.Variable | Type.Variable] = term match {
      case Term.Constant(_) => Set.empty
      case v: Term.Variable => Set(v)
      case Term.Function(_, args*) => args.flatMap(_.termVariables).toSet
      case Term.Type(typ) => typ.typeVariables
      case r @ Term.Range(_, _, _, _, maxIndex, _) =>
        val indexVars = maxIndex.asVariable.map(v => Term.Variable(v.name): Term.Variable).toSet
        indexVars ++ getRangeElems(r, _.termVariables).toSet
    }

    def types: Set[Type] = term match {
      case Term.Constant(_) => Set()
      case Term.Variable(_) => Set()
      case Term.Function(_, args*) => args.flatMap(_.types).toSet
      case Term.Type(t) => Set(t)
      case r: Term.Range => getRangeElems(r, _.types).toSet
    }

  extension (asrt: Assertion)

    def typeVariables: Set[String] = types.flatMap(_.variables)
    
    def termVariables: Set[String] = asrt match {
      case HasType(e, _) => e.variables
    }

    def types: Set[Type] = Set(asrt match {
      case HasType(term, typ) => typ
    })

  extension (prem: Premise)

    private def combineWithoutWildcard(fromVars: Set[String], toVars: Set[String]): Set[String] = {
      // `to` premise may contain wildcard index variables, e.g. e_n, which
      // don't correspond to a *concrete* variable in scope, so we never
      // want them to be listed.
      val fromIndexedVars = fromVars.collect(extractIndex.unlift).map(_._1)
      val toVarsWithoutWildcardIndex = toVars
        .filter(v => extractIndex(v) match {
          case Some((name, idxStr)) => !fromIndexedVars.contains(name) || idxStr.toIntOption.isDefined
          case None => true
        })

      fromVars ++ toVarsWithoutWildcardIndex
    }

    def termVariables: Set[String] = prem match {
      case Judgement(env, assertion) => assertion.termVariables ++ env.termVariables
      case JudgementRange(from, to) => combineWithoutWildcard(from.termVariables, to.termVariables)
    }

    def termTypeVariables: Set[String] = prem match {
      case Judgement(_, assertion) => assertion.typeVariables 
      case JudgementRange(from, to) => combineWithoutWildcard(from.termTypeVariables, to.termTypeVariables)
    }

    def envTypeVariables: Set[String] = prem match {
      case Judgement(env, _) => env.typeVariables
      case JudgementRange(from, to) => combineWithoutWildcard(from.envTypeVariables, to.envTypeVariables)
    }

    def types: Set[Type] = prem match {
      case Judgement(env, asrt) => env.types ++ asrt.types
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

    def typeVariables: Iterable[Type.Variable] = typ match {
      case Type.Named(_) => Set.empty
      case v: Type.Variable => Set(v)
      case Type.Composite(_, args*) => args.flatMap(_.typeVariables).toSet
      case r: Type.Range => getRangeElems(r, _.typeVariables).toSet
    }

  extension [TTerm <: TermOps[TTerm, TConstant], TConstant](range: TermRange[TTerm])

    def toConcrete(createFunction: (String, TTerm, TTerm) => TTerm): Option[TTerm] = range.maxIndex match {
      case Index.Number(maxIndex) if range.minIndex < maxIndex =>
        val args = range.seed.toSeq ++ (range.minIndex to maxIndex).map(i => range.template.replaceIndex(range.cursor, i.toString))
        val funTerm = args.drop(1).foldLeft(args.head) { (l, r) => createFunction(range.function, l, r) }
        Some(funTerm)
      case _ => None
    }
