package tyes.interpreter

import scala.util.matching.Regex
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*

object TyesInterpreter:

  private val DefaultEnv = 
    val defaultVarPart = EnvironmentPart.Variable("$env")
    Environment(Seq(defaultVarPart))
  
  private def getSelfOrDefaultIfEmpty(env: Environment, default: Environment): Environment =
    if env.parts.isEmpty 
    then default
    else env

  def typecheck(tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term, termEnv: Map[String, Type]): Option[Type] = ruleDecl.conclusion match {
    case Judgement(metaEnv, HasType(metaTerm, typ)) =>
      metaTerm.matches(term).flatMap { (termSubst) =>
        // replace the vars we already unified in the term
        // TODO: review assumption that only string constants that are directly unified are variables
        val termVarSubst = termSubst.collect { case (k, Term.Constant(varName: String)) => k -> varName }
        val typeVarSubst = termSubst.collect { case (k, Term.Type(t)) => k -> t }
        val refinedMetaEnv = getSelfOrDefaultIfEmpty(metaEnv, DefaultEnv)
          .substitute(EnvironmentMatch(termVarSubst, typeVarSubst))

        // match the conclusion env to the rule to see if it's applicable at all
        refinedMetaEnv.matches(termEnv).flatMap { case m @ EnvironmentMatch(envTermVarSubst, envTypeVarSubst, envVarSubst) =>
          val envTermSubst = envTermVarSubst.mapValues(Term.Constant(_)).toMap
          val refinedMetaTerm = metaTerm.substitute(envTermSubst)

          // build variable substitutions considering info from term and environment
          val allVarSubst = envTermVarSubst ++ termVarSubst
          val allTypeVarSubst = envTypeVarSubst ++ typeVarSubst
          val allTermSubst = envTermSubst ++ termSubst

          // check all premises hold, while unifying possible type variables
          val premTypeCheckResult = ruleDecl.premises.foldLeft(Option(allTypeVarSubst)) { 
            // if one of the premises failed, propagate failure
            case (None, _) => None 
            // otherwise, check the next premise
            case (Some(typeVarEnv), judg: Judgement) =>
              val premEnvMatch = EnvironmentMatch(allVarSubst, typeVarEnv, envVarSubst)
              typecheck(tsDecl, judg, premEnvMatch, refinedMetaEnv, allTermSubst)
            case (Some(typeVarEnv), JudgementRange(from, to)) =>
              val Judgement(fromEnv, HasType(Term.Variable(fromVar), fromTyp)) = from
              val Judgement(toEnv, HasType(Term.Variable(toVar), toTyp)) = to

              val Array(fromIdent, fromIdxStr) = fromVar.split("_")
              val Array(toIdent, toIdxStr) = toVar.split("_")
              assert(fromIdent == toIdent, "Both from and to premise must share the judgement var minus index") // todo: generalize to matched terms?
              
              var typeVarsToReplace = getIndexedTypeVarsToReplace(fromTyp, toTyp, fromIdxStr, toIdxStr)
              var termVarsToReplace = Set[String]()

              fromEnv.parts.zip(toEnv.parts).foreach({
                case (fv: EnvironmentPart.Variable, tv: EnvironmentPart.Variable) => assert(fv == tv)
                case (EnvironmentPart.Bindings(fbs), EnvironmentPart.Bindings(tbs)) =>
                  fbs.zip(tbs).foreach({
                    case (Binding.BindName(fn, ft), Binding.BindName(tn, tt)) =>
                      assert(fn == tn)
                      typeVarsToReplace ++= getIndexedTypeVarsToReplace(ft, tt, fromIdxStr, toIdxStr)
                    case (Binding.BindVariable(fv, ft), Binding.BindVariable(tv, tt)) =>
                      val fTermVarArr = fv.split("_", 2)
                      val tTermVarArr = tv.split("_", 2)
                      assert(fTermVarArr.length == tTermVarArr.length, s"Both from and to premise matching vars must either have index or not: got ${fv} and ${tv}")
                      assert(fTermVarArr(0) == tTermVarArr(0), s"Both from and to premise must share the same vars minus index: got ${fv} and ${tv}")
            
                      if fTermVarArr.length > 1 then
                        assert(fTermVarArr.length == 2, s"Only one level of indexes supported: $fTermVarArr")
                        if fTermVarArr(1) != tTermVarArr(1) then
                          assert(fTermVarArr(1) == fromIdxStr, s"From premise var indexes must match term var indexes: got ${fTermVarArr(1)}, expected $fromIdxStr")
                          assert(tTermVarArr(1) == toIdxStr, s"To premise var indexes must match term var indexes: got ${tTermVarArr(1)}, expected $toIdxStr")
                          termVarsToReplace += fTermVarArr(0)

                      typeVarsToReplace ++= getIndexedTypeVarsToReplace(ft, tt, fromIdxStr, toIdxStr)
                    case _ => assert(false, "Both from and to premise environments must match minus var indexes")
                  })
                case _ => assert(false, "Both from and to premise environments must match minus var indexes")
              })

              val fromIdx = fromIdxStr.toInt
              val toIdx = toIdxStr.toIntOption.getOrElse(Int.MaxValue)
              // Assumption all the variable indexes are bound in the conclusion
              val premsToConsider = for
                (termVar, term) <- allTermSubst
                if termVar.matches(Regex.quote(fromIdent) + "_" + ".+")
                currIdx = termVar.split("_")(1).toInt
                if currIdx >= fromIdx && currIdx <= toIdx
              yield
                val idxTypeVarSubst = typeVarsToReplace
                  .map(n => s"${n}_${fromIdxStr}" -> Type.Variable(s"${n}_${currIdx}"))
                  .toMap

                val idxTermVarSubst = termVarsToReplace
                  .map(n => s"${n}_${fromIdxStr}" -> s"${n}_${currIdx}")
                  .toMap
                
                Judgement(
                  fromEnv.remap(idxTermVarSubst).substitute(EnvironmentMatch(typeVarSubst = idxTypeVarSubst)),
                  HasType(term, fromTyp.substitute(idxTypeVarSubst))
                )

              premsToConsider.foldLeft(Option(typeVarEnv)) {
                case (None, _) => None
                case (Some(pTypeVarEnv), judg) =>
                  val premEnvMatch = EnvironmentMatch(allVarSubst, pTypeVarEnv, envVarSubst)
                  typecheck(tsDecl, judg, premEnvMatch, refinedMetaEnv, allTermSubst)
              }
          }

          premTypeCheckResult.map(typ.substitute(_))
        }
      }
  }

  private def getIndexedTypeVarsToReplace(fromTyp: Type, toTyp: Type, fromIdxStr: String, toIdxStr: String): Iterable[String] =
    fromTyp.unifies(toTyp) match { 
      case Some(subst) =>
        for
          (fTypeVarStr, Type.Variable(tTypeVarStr)) <- subst: @unchecked
          fTypeVarArr = fTypeVarStr.split("_", 2)
          tTypeVarArr = tTypeVarStr.split("_", 2)
          _ = assert(fTypeVarArr.length == tTypeVarArr.length, s"Both from and to premise matching type vars must either have index or not: got ${fTypeVarStr} and ${tTypeVarStr}")
          _ = assert(fTypeVarArr(0) == tTypeVarArr(0), s"Both from and to premise must share the same type vars minus index: got ${fTypeVarStr} and ${tTypeVarStr}")
          if fTypeVarArr.length > 1
        yield
          assert(fTypeVarArr.length == 2, s"Only one level of indexes supported: $fTypeVarArr")
          assert(fTypeVarArr(1) == fromIdxStr, s"From premise type var indexes must match term var indexes: got ${fTypeVarArr(1)}, expected $fromIdxStr")
          assert(tTypeVarArr(1) == toIdxStr, s"To premise type var indexes must match term var indexes: got ${tTypeVarArr(1)}, expected $toIdxStr")
          fTypeVarArr(0)
    }

  def typecheck(
    tsDecl: TypeSystemDecl,
    judgement: Judgement,
    premEnvMatch: EnvironmentMatch,
    refinedMetaEnv: Environment,
    allTermSubst: Map[String, Term]
  ): Option[Map[String, Type]] =
    val Judgement(premMetaEnv, HasType(premTerm, premTyp)) = judgement: @unchecked
    
    // replace the term and type variables we already know in the premise env and then produce an
    // actual term env out of it
    val premEnvOpt = getSelfOrDefaultIfEmpty(premMetaEnv, refinedMetaEnv).substitute(premEnvMatch).toConcrete

    val refinedPremTerm = premTerm.substitute(allTermSubst)
    val resTyp = premEnvOpt.flatMap(premEnv => typecheck(tsDecl, refinedPremTerm, premEnv))
    for
      resT <- resTyp
      premTypeSubst <- premTyp.substitute(premEnvMatch.typeVarSubst).matches(resT)
    yield
      // if the premise expected type matched the obtained, update the type env with any new unifications
      premEnvMatch.typeVarSubst ++ premTypeSubst

  def typecheck(tsDecl: TypeSystemDecl, term: Term, env: Map[String, Type]): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term, env)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert, Map())
