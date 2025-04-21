package tyes.compiler.old

import java.nio.file.Path
import scala.collection.mutable
import tyes.compiler.Orderings.given
import tyes.compiler.TyesCompiler
import tyes.compiler.TyesEnvDesugarer
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*

private class TyesCodeGenerator(defaultEnvName: String = "env"):

  val defaultEnvVarExpr = getEnvFreshVarName(defaultEnvName)

  def compileValue(value: Any): String = value match {
      case i: Int => i.toString
      case s: String => '\"' + s + '\"'
      case _ => throw new Exception(s"No compilation defined for constants of type ${value.getClass().getName()}: $value")
  }

  def compile(term: Term, typSubst: Map[String, String]): String = term match {
    case Term.Constant(value) => compileValue(value)
    case Term.Variable(name, None) => name
    case Term.Function(name, args*) => name + args.map(compile(_, typSubst)).mkString("(", ", ", ")")
    case Term.Type(typ) => TypeCodeGenerator.compile(typ, typSubst) 
  }
  
  def generateBinding(binding: Binding): (String, Type) = binding match {
    case Binding.BindName(name, typ) => (s"\"$name\"", typ)
    case Binding.BindVariable(name, typ) => (name, typ)
  }

  def compile(binding: Binding, typSubst: Map[String, String]): String =
    val (varNameExpr, typ) = generateBinding(binding)
    s"$varNameExpr -> ${TypeCodeGenerator.compile(typ, typSubst)}"

  def compile(env: EnvironmentPart, typSubst: Map[String, String]): String = env match {
    case EnvironmentPart.Bindings(bindings) =>
      val entryExprs = bindings.map(compile(_, typSubst))
      entryExprs.mkString("Map(", ", ", ")")
    case EnvironmentPart.Variable(name) =>
      if name != defaultEnvName then
        throw new Exception("Compilation of rules with distinct environment variables is not supported")

      defaultEnvVarExpr
  }

  def compile(env: Environment, typSubst: Map[String, String]): String =
    assert(!env.parts.isEmpty)
    env.parts.map(compile(_, typSubst)).mkString(" ++ ")

  def getEnvFreshVarName(base: String): String = base.decapitalize

  def getFreshVarName(base: String): String = s"_$base"

  def getFreshVarName(base: String, index: Int): String = s"${getFreshVarName(base)}${index + 1}"

  def extractTemplate(term: Term): Term = term match {
    case Term.Function(fnName, args*) =>
      val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
        arg match {
          case Term.Variable(_, None) => arg
          case Term.Constant(_) => Term.Variable(getFreshVarName("c", idx), None)
          case Term.Function(_, _*) => Term.Variable(getFreshVarName("e", idx), None)
          case Term.Type(typ) => Term.Type(typ match {
            case Type.Variable(_) => typ
            case Type.Named(_) => Type.Variable(getFreshVarName("ct", idx))
            case Type.Composite(_, _*) => Type.Variable(getFreshVarName("ct", idx))
          })
        }
      }
      Term.Function(fnName, argsAsVariables*)
    case _ => term
  }

  def getRulesByConstructor(tsDecl: TypeSystemDecl): Map[Term, Seq[RuleDecl]] =
    (for
      r <- tsDecl.rules
      HasType(term, _) = r.conclusion.assertion
    yield
      val constructor = extractTemplate(term)
      (constructor, r)
    ).groupMap(_._1)(_._2)

  def compileTypecheck(tsDecl: TypeSystemDecl, indent: String): String =
    val rulesByConstructor = getRulesByConstructor(tsDecl)
    (
      for (c, rs) <- rulesByConstructor.toSeq.sortBy((c, _) => c)
      yield
        val caseBody = rs match {
          // Special case for catch'all rules with no premises
          case Seq(r @ RuleDecl(_, Seq(), Judgement(Environment(Seq(EnvironmentPart.Variable(_))), HasType(Term.Variable(_, _), _)))) => 
            compileRule(r, Map(), Seq(), Map(), indent)
          case _ =>
            val inductionDeclNames = rs.flatMap(_.premises).zipWithIndex.toMap.mapValues(idx => getFreshVarName("t", idx))
            val (destructureDeclsRaw, typeVarsFromDestructuring) = rs.foldLeft((Seq[String](), Map[String, String]())) { 
              case ((decls, typeVarMap), r) => {
                val (extraDecls, extraTypeVars) = compileDestructurings(r)
                (decls ++ extraDecls, typeVarMap ++ extraTypeVars)
              }
            }
            val destructureDecls = destructureDeclsRaw.map(CodeSimplifier.simplify).map(line => s"\r\n$indent  $line").mkString
            val inductionDecls = new mutable.StringBuilder()
            val condsPerRule = mutable.Map[RuleDecl, Iterable[String]]()
            val typeVarEnvPerRule = mutable.Map[RuleDecl, Map[String, String]]()
            
            for r <- rs do
              val Judgement(conclEnv, HasType(concl, conclTyp)) = r.conclusion

              val typeVarsFromEnv = Map.from(
                for
                  case EnvironmentPart.Bindings(bindings) <- r.conclusion.env.parts
                  case (varNameExpr, Type.Variable(typVarName)) <- bindings.map(generateBinding)
                yield
                  typVarName -> s"$defaultEnvVarExpr.get($varNameExpr).toRight(s\"'$${$varNameExpr}' not found\")"
              )

              val typeVarsFromTerm = Map.from(
                (for 
                  typ <- concl.types 
                  if !typ.isGround
                yield typ match {
                    case Type.Named(name) => throw new Exception(s"Unexpected ground type: $name")
                    case Type.Variable(name) => Seq(name -> s"$name.toRight(\"No type provided\")")
                    case _: Type.Composite => Seq() // type vars already yielded by destructuring
                  }
                ).flatten
              )

              val declaredTypeVars = typeVarsFromTerm ++ typeVarsFromDestructuring

              val ruleTypeVarEnv = r.premises.foldLeft(typeVarsFromEnv) {
                case (typeVarEnv, judg @ Judgement(premEnv, HasType(premTerm, premTyp))) =>
                  val premTypVarName = inductionDeclNames(judg)
                  val premTypecheckExpr = compileInductionCall(judg, declaredTypeVars ++ typeVarEnv, c.variables)
                  
                  inductionDecls ++= s"\r\n$indent  val $premTypVarName = $premTypecheckExpr"

                  val premTypVarEnv = Map.from(
                    for (Type.Variable(typVarName), getter) <- TypeCodeGenerator.genTypeVariableGetters(premTyp, baseGetter = "")
                    yield declaredTypeVars.getOrElse(typVarName, typVarName) -> s"$premTypVarName$getter"
                  )
                  
                  // Add envs together preserving the oldest entries
                  premTypVarEnv ++ typeVarEnv
              }

              val conds = generateSyntacticConditions(c, concl, typeVarsFromDestructuring ++ ruleTypeVarEnv)
                ++ generateEnvironmentPartConditions(conclEnv, typeVarsFromDestructuring ++ ruleTypeVarEnv)
                ++ generateTermTypeConditions(declaredTypeVars, ruleTypeVarEnv)

              condsPerRule += r -> conds
              typeVarEnvPerRule += r -> (typeVarsFromDestructuring ++ ruleTypeVarEnv)

            val defaultCase = s"\r\n$indent    ${getTypeErrorString("exp")}"
            val ruleEvaluations = rs.foldRight(defaultCase) { (r, res) =>
              compileRule(r, inductionDeclNames, condsPerRule(r), typeVarEnvPerRule(r), indent + "  ") + " " + res
            }
            s"${destructureDecls}${inductionDecls}\r\n$indent  $ruleEvaluations"
        }

        s"case ${compile(c, Map.from(for v <- c.typeVariables yield v -> v))} => $caseBody" 
    ).mkString(s"\r\n$indent")

  def getTypeErrorString(expVarName: String): String = s"Left(s\"TypeError: no type for `$$$expVarName`\")"

  def compileInductionCall(judg: Judgement, typeVarEnv: Map[String, String], declaredVariables: Set[String]): String =
    val Judgement(env, HasType(term, typ)) = judg
    val buildTypecheckExpr = (typEnv: Map[String, String]) => s"typecheck(${compile(term, typEnv)}, ${compile(env, typEnv)})"

    val extraTypeVars = env.typeVariables.map(name => 
      if declaredVariables.contains(name)
      then name -> getFreshVarName(name)
      else name -> name
    ).toSeq

    var typecheckExpr = buildTypecheckExpr(typeVarEnv ++ extraTypeVars)
    for case (typeVarName, lambdaVarName) <- extraTypeVars.reverse do 
      val typStr = typeVarEnv.getOrElse(typeVarName, throw new Exception(s"Unbound type variable: $typeVarName"))  
      typecheckExpr = s"$typStr.flatMap($lambdaVarName => ${typecheckExpr})"

    // Destructure the variables from the term *if* they are not already declared
    for varName <- term.variables.diff(declaredVariables) do
      typecheckExpr = s"${getFreshVarName(varName)}.flatMap($varName => $typecheckExpr)"
    
    // Destructure the variables from the environment *if* they are not already declared
    val nonDeclaredEnvTermVariables = env.termVariables.diff(declaredVariables)
    if nonDeclaredEnvTermVariables.nonEmpty then
      val varName = nonDeclaredEnvTermVariables.head
      typecheckExpr = s"${getFreshVarName(varName)}.flatMap($varName => $typecheckExpr)"

    // Specialize the result type if it is composite
    typecheckExpr = typ match {
      case tc @ Type.Composite(_, _*) => s"$typecheckExpr.flatMap(${TypeCodeGenerator.genSpecializationFunction(tc)})"
      case _ => typecheckExpr
    }

    return typecheckExpr


  def compileDestructurings(rule: RuleDecl): (Seq[String], Map[String, String]) = 
    val concl = rule.conclusion
    val (asrtDecls, asrtDeclsTypeVars) = compileDestructurings(concl.assertion).unzip
    val (envDecls, envDeclsTypeVars) = compileDestructurings(concl.env).unzip
    (
      asrtDecls ++ envDecls, 
      (asrtDeclsTypeVars ++ envDeclsTypeVars).flatten.toMap
    )
  
  def compileDestructurings(asrt: Assertion): Seq[(String, Map[String, String])] = asrt match {
    case HasType(Term.Function(_, fnArgs*), _) if fnArgs.exists(_.isInstanceOf[Term.Type]) =>
      for 
        case (Term.Type(typ: Type.Composite), idx) <- fnArgs.zipWithIndex
        res <- compileDestructuring(typ, getFreshVarName("ct", idx))
      yield
        res

    case HasType(Term.Function(_, fnArgs*), _) if fnArgs.exists(_.variables.nonEmpty) =>
      for case (Term.Function(fnName, Term.Variable(metaVarName, None)), idx) <- fnArgs.zipWithIndex yield
        (
          s"val ${getFreshVarName(metaVarName)} = ${getFreshVarName("e", idx)} match { case $fnName(v) => Right(v) ; case _ => Left(\"Not a $fnName\") }",
          Map()
        )
    case _ => Seq()
  }

  def compileDestructurings(env: Environment): Seq[(String, Map[String, String])] =
    for 
      case EnvironmentPart.Bindings(bs) <- env.parts
      case (varNameExpr, typ: Type.Composite) <- bs.map(generateBinding)

      envAccessExpr = s"$defaultEnvVarExpr.get($varNameExpr)"
      errorMessageExpr = s"s\"'$${$varNameExpr}' is not in scope\""
      res <- compileDestructuring(typ, envAccessExpr, Some(errorMessageExpr))
    yield
      res

  def compileDestructuring(
    compositeTyp: Type.Composite,
    targetObjExpr: String,
    errorExprOpt: Option[String] = None
  ): Option[(String, Map[String, String])] =
    if compositeTyp.args.forall(_.isGround) then
      None
    else
      val typGenName = TypeCodeGenerator.genName(compositeTyp.name)
      val typArgsVarNames = for case Type.Variable(name) <- compositeTyp.args yield name -> getFreshVarName(name)
      var errorArgExpr = errorExprOpt.map(", " + _).getOrElse("") 
      Some((
        s"val Seq(${typArgsVarNames.map(_._2).mkString(", ")}) = destructure[$typGenName]($targetObjExpr$errorArgExpr)",
        Map.from(typArgsVarNames)
      ))

  private def getTypeEnvForRule(rule: RuleDecl, inductionDeclNames: PartialFunction[Judgement, String]): Map[String, String] =
    val Judgement(conclEnv, HasType(conclTerm, _)) = rule.conclusion

    val typeVarsFromPremises = 
      for 
        case judg @ Judgement(_, HasType(_, premTyp)) <- rule.premises.reverse // reversed, so the first occurrence takes precedence
        (typVar, getter) <- TypeCodeGenerator.genTypeVariableGetters(premTyp)
      yield 
        typVar.name -> s"${inductionDeclNames(judg)}$getter"
        
    val typeVarsFromTerm = 
      (for 
        typ <- conclTerm.types
        if !typ.isGround
      yield typ match {
        case Type.Named(name) => throw new Exception(s"Unexpected ground type: $name")
        case Type.Variable(typVarName) => Seq(typVarName -> s"$typVarName.get")
        case Type.Composite(_, argTypes*) =>
          for 
            argTyp <- argTypes
            (typVar, getter) <- TypeCodeGenerator.genTypeVariableGetters(argTyp)
          yield 
            typVar.name -> s"${getFreshVarName(typVar.name)}$getter"
      }).flatten

    val typeVarsFromEnv =
      for 
        case EnvironmentPart.Bindings(bindings) <- conclEnv.parts
        case (nameVarExpr, Type.Variable(typVarName)) <- bindings.map(generateBinding)
      yield
        typVarName -> s"$defaultEnvVarExpr($nameVarExpr)"
    
    Map.from(typeVarsFromPremises ++ typeVarsFromTerm ++ typeVarsFromEnv)

  def compileRule(
    rule: RuleDecl,
    inductionDeclNames: PartialFunction[Judgement, String],
    conds: Iterable[String],
    typeVarEnv: Map[String, String],
    indent: String
  ): String =
    val Judgement(conclEnv, HasType(_, conclTyp)) = rule.conclusion
    
    val premises = 
      for case judg @ Judgement(_, HasType(_, premTyp)) <- rule.premises
      yield (inductionDeclNames(judg), premTyp)
    
    val extraPremises = 
      for
        case EnvironmentPart.Bindings(bindings) <- conclEnv.parts
        case (nameVarExpr, typ: Type.Variable) <- bindings.map(generateBinding)
      yield 
        (s"Right($defaultEnvVarExpr($nameVarExpr))", typ)

    var typVarSubst = getTypeEnvForRule(rule, inductionDeclNames)
    for (k, v) <- typeVarEnv if !typVarSubst.contains(k) do
      typVarSubst += k -> s"$v.getOrElse(???)"

    val body = generateTypeCheckIf(premises ++ extraPremises, conclTyp, typVarSubst, leaveOpen = conds.isEmpty)
    
    if conds.isEmpty then
      body.mkString("\r\n" + indent)
    else
      (Seq(
        s"if ${conds.mkString(" && ")} then",
      ) ++ body.map("  " ++ _) ++ Seq(
        s"else "
      )).mkString("\r\n" + indent)

  def generateSyntacticConditions(constructor: Term, conclTerm: Term, typSubst: Map[String, String]): Iterable[String] =
    val subst = constructor.matches(conclTerm).get.filter((_, v) => v.isGround)

    subst.map { (n, v) => s"$n == ${compile(v, typSubst)}" }

  def generateEnvironmentPartConditions(env: Environment, typEnv: Map[String, String]): Iterable[String] =
    assert(!env.parts.isEmpty)

    val sizeOpt = 
      if !env.envVariables.isEmpty
      then None
      else Some(env.parts.collect({ case EnvironmentPart.Bindings(bs) => bs.length }).sum)

    val sizeCondition = sizeOpt.map(size => 
      if size == 0 
      then s"$defaultEnvVarExpr.isEmpty"
      else s"$defaultEnvVarExpr.size == $size"
    )

    sizeCondition.toSeq ++ env.parts.flatMap(part => generateEnvironmentPartConditions(part, typEnv))

  def generateEnvironmentPartConditions(env: EnvironmentPart, typEnv: Map[String, String]): Iterable[String] = env match {
    case EnvironmentPart.Bindings(bindings) =>  
      // For each binding, if we have an expected type, check right away, otherwise just check for containment
      for 
        b <- bindings
        (varNameExpr, typ) = generateBinding(b)
        cond <- typ match {
          case t: Type.Named => Seq(s"$defaultEnvVarExpr.get($varNameExpr) == Some(${TypeCodeGenerator.compile(t)})")
          case Type.Variable(_) => Seq(s"$defaultEnvVarExpr.contains($varNameExpr)")
          case t: Type.Composite =>
            for 
              v <- t.variables
              if typEnv.contains(v)
            yield
              s"${typEnv(v)}.isRight"
        }
      yield
        cond
    case EnvironmentPart.Variable(_) => 
      Seq()
  }

  def generateTermTypeConditions(termTypeVars: Map[String, String], ruleTypeEnv: Map[String, String]): Iterable[String] =
    // For all conclusion term variables that are also used in the rule's expected type
    // generate the equality check between both
    for case (termTypeVar, Some(ruleTypeValue)) <- termTypeVars.mapValues(ruleTypeEnv.get) 
    yield s"${termTypeVars(termTypeVar)} == $ruleTypeValue"

  def generateTypeCheckIf(premisesToCheck: Seq[(String, Type)], conclTyp: Type, typVarSubst: Map[String, String], leaveOpen: Boolean): Seq[String] =
    /** General idea: the first occurence of a type variable must be bound to that type and then used
      * e.g. 
      * 
      *  rule infers e1 + e2 + e3 + e4 : v
      *     if e1 : t 
      *    and e2 : v 
      *    and e3 : t 
      *    and e4 : int
      * 
      * would be equivalent to
      * 
      *  rule infers e1 + e2 + e3 + e4 : typeof(e2)
      *    if e1 : typeof(e1) and e2 : typeof(e2) and e3 : typeof(e1) and e4 : int
      * 
      * which in turn leads to the simplification of the premises to
      * 
      *    e1 has a type and e2 has a type and e3 : typeof(e1) and e4 : int
      * 
      * which in turn compiles to something like
      * 
      *    val t1 = typecheck(e1)
      *    ...
      *    if t1.isRight && t2.isRight && t3 == t1 && t4 == Right(Type.Int) then
      *      t2
      *    else
      *      Left(s"...")
    * */
    val (premConds, _) = premisesToCheck.foldLeft(Seq[String](), Map[String, String]()) {
      case ((conds, typCheckResEnv), (typCheckRes, premTyp)) =>
        val (newConds, newTypCheckResEnv) = TypeCodeGenerator.genTypeConditions(typCheckRes, premTyp, typCheckResEnv)
        val simplifiedNewConds = newConds.map(CodeSimplifier.simplify).filter(_ != "true")
        ((conds ++ simplifiedNewConds).distinct, newTypCheckResEnv)
    }
    
    val conclTypRes = CodeSimplifier.simplify(s"Right(${TypeCodeGenerator.compile(conclTyp, typVarSubst)})")

    if premConds.isEmpty
      // simplification of the special case of a single "isRight" condition 
      || (premConds.length == 1 
          && conclTyp.isInstanceOf[Type.Variable]
          && !leaveOpen)
    then  
      Seq(conclTypRes)
    else
      val ifCode = Seq(
        premConds.mkString("if ", " && ", " then"),
        s"  $conclTypRes"
      )
      val elseCode = 
        if leaveOpen 
        then Seq("else") 
        else Seq("else", s"  ${getTypeErrorString("exp")}")
      
      ifCode ++ elseCode
  
  private def compileTypeSystem(tsDecl: TypeSystemDecl): String =
    s"""
    import tyes.runtime.old.*
    import tyes.runtime.Type
    import example.*
    
    object ${TyesCodeGenerator.getTypeSystemObjectName(tsDecl)} extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      ${TypeCodeGenerator.compile(tsDecl, "      ")}
    
      def typecheck(exp: LExpression[Type], $defaultEnvVarExpr: Map[String, Type]): Either[String, Type] = exp match {
        ${compileTypecheck(tsDecl, "        ")}
        case _ => ${getTypeErrorString("exp")}
      }
    """

object TyesCodeGenerator extends TyesCompiler:

  def compile(tsDecl: TypeSystemDecl): (Path, String) =
    val commonEnvName = TyesEnvDesugarer.inferEnvVarName(tsDecl).getOrElse("env")
    val desugaredTsDecl = TyesEnvDesugarer(commonEnvName).desugar(tsDecl)
    (
      getFileName(desugaredTsDecl),
      TyesCodeGenerator(commonEnvName).compileTypeSystem(desugaredTsDecl)
    )

  def getTypeSystemObjectName(tsDecl: TypeSystemDecl): String = tsDecl.name.getOrElse("") + "TypeSystem"

  private def getFileName(tsDecl: TypeSystemDecl) = Path.of(s"${getTypeSystemObjectName(tsDecl)}.scala")
