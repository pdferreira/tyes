package tyes.compiler

import scala.collection.mutable
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*

object TyesCodeGenerator:

  def getTypeSystemObjectName(tsDecl: TypeSystemDecl): String = tsDecl.name.getOrElse("") + "TypeSystem"

  def compile(term: Term): String = term match {
    case Term.Constant(value) => value match {
      case i: Int => i.toString
      case s: String => '\"' + s + '\"'
      case _ => throw new Exception(s"No compilation defined for constants of type ${value.getClass().getSimpleName}: $value")
    }
    case Term.Variable(name) => name
    case Term.Function(name, args*) => name + args.map(compile).mkString("(", ", ", ")")
  }
  
  def generateBinding(binding: Binding): (String, Type) = binding match {
    case Binding.BindName(name, typ) => (s"\"$name\"", typ)
    case Binding.BindVariable(name, typ) => (name, typ)
  }

  def compile(binding: Binding, typSubst: Map[String, String]): String =
    val (varNameExpr, typ) = generateBinding(binding)
    typ match {
      case t @ Type.Named(_) => 
        s"$varNameExpr -> ${compileNamedType(t)}"
      case Type.Variable(varTypeName) => 
        s"$varNameExpr -> $varTypeName"
    }

  def compile(env: EnvironmentPart, typSubst: Map[String, String]): String = env match {
    case EnvironmentPart.Bindings(bindings) =>
      val entryExprs = bindings.map(compile(_, typSubst))
      entryExprs.mkString("Map(", ", ", ")")
    case EnvironmentPart.Variable(_) =>
      // TODO: take into account env var name 
      // getEnvFreshVarName(name)
      "env"
  }

  def compile(env: Environment, typSubst: Map[String, String]): String =
    if env.parts.isEmpty then
      "env"
    else
      env.parts.map(compile(_, typSubst)).mkString(" ++ ")

  def compileNamedType(typ: Type.Named): String = s"Type.${typ.name.capitalize}"

  def getEnvFreshVarName(base: String): String =
    if base.head.isLower then
      getFreshVarName(base)
    else
      getEnvFreshVarName(base.head.toLower + base.substring(1))

  def getFreshVarName(base: String): String = s"_$base"

  def getFreshVarName(base: String, index: Int): String = s"${getFreshVarName(base)}${index + 1}"

  def compileTypecheck(tsDecl: TypeSystemDecl, indent: String): String =
    val rulesByConstructor = 
      (for
        r <- tsDecl.rules
        HasType(term, _) = r.conclusion.assertion
      yield
        val constructor = term match {
          case Term.Function(fnName, args*) =>
            val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
              arg match {
                case Term.Variable(_) => arg
                case Term.Constant(_) => Term.Variable(getFreshVarName("c", idx))
                case Term.Function(_, _*) => Term.Variable(getFreshVarName("e", idx))
              }
            }
            Term.Function(fnName, argsAsVariables*)
          case _ => term
        }
        (constructor, r)
      ).groupMap(_._1)(_._2)
    (
      for (c, rs) <- rulesByConstructor
      yield
        val caseBody = rs match {
          // Special case for catch'all rules with no premises
          case Seq(r @ RuleDecl(_, Seq(), Judgement(Environment(Seq()), HasType(Term.Variable(_), _)))) => 
            compileRule(c, r, Map(), indent)
          case _ =>
            val inductionDeclNames = rs.flatMap(_.premises).zipWithIndex.toMap.mapValues(idx => getFreshVarName("t", idx))
            val destructureDecls = rs.flatMap(compileDestructurings).map(line => s"\r\n$indent  $line").mkString
            val inductionDecls = new mutable.StringBuilder()
            // TODO: Type variables are now properly scoped per rule, but now common premises don't get merged...
            // This needs a big rewrite
            for r <- rs do
              val typeVarsFromEnv = 
                for
                  case EnvironmentPart.Bindings(bindings)  <- r.conclusion.env.parts
                  case (varNameExpr, Type.Variable(typVarName)) <- bindings.map(generateBinding)
                yield
                  typVarName -> s"env.get($varNameExpr).toRight(s\"'$${$varNameExpr}' not found\")"
              
              r.premises.foldLeft(Map.from(typeVarsFromEnv)) {
                case (typeVarEnv, judg @ Judgement(_, HasType(_, premTyp))) =>
                  val typVarName = inductionDeclNames(judg)
                  val typecheckExpr = compileInductionCall(judg, typeVarEnv, c.variables)
                  
                  inductionDecls ++= s"\r\n$indent  val $typVarName = $typecheckExpr"
                  premTyp match {
                    case Type.Named(_) => typeVarEnv
                    case Type.Variable(premTypVar) =>
                      if typeVarEnv.contains(premTypVar) then
                        typeVarEnv
                      else
                        typeVarEnv + (premTypVar -> typVarName) 
                  }
              }

            val defaultCase = s"\r\n$indent    ${getTypeErrorString("exp")}"
            val ruleEvaluations = rs.foldRight(defaultCase)((r, res) => compileRule(c, r, inductionDeclNames, indent + "  ") + " " + res)
            s"${destructureDecls}${inductionDecls}\r\n$indent  $ruleEvaluations"
        }
        s"case ${compile(c)} => $caseBody" 
    ).mkString(s"\r\n$indent")

  def getTypeErrorString(expVarName: String): String = s"Left(s\"TypeError: no type for `$$$expVarName`\")"

  def compileInductionCall(judg: Judgement, typeVarEnv: Map[String, String], declaredVariables: Set[String]): String =
    val Judgement(env, HasType(term, _)) = judg
    var typecheckExpr = s"typecheck(${compile(term)}, ${compile(env, typeVarEnv)})"

    // TODO: Hard-coded for a single variable cases for now. Probably not worth generalizing before the generated code
    // structure (and code generation strategy) is reviewed.
    for case Term.Function(_, Term.Variable(varName)) <- Seq(term) do
      typecheckExpr = s"${getFreshVarName(varName)}.flatMap($varName => $typecheckExpr)"
    
    val envTypeVariables = env.typeVariables
    if envTypeVariables.nonEmpty then
      val varTypeName = envTypeVariables.head
      val typStr = typeVarEnv.getOrElse(varTypeName, throw new Exception(s"Unbound type variable: $varTypeName"))
      typecheckExpr = s"$typStr.flatMap($varTypeName => $typecheckExpr)"
    
    // Destructure the variables from the environment *if* they are not already declared
    val envTermVariables = env.termVariables
    if envTermVariables.diff(declaredVariables).nonEmpty then
      val varName = envTermVariables.head
      // TODO: validate if the variable is supposed to be bound here at all, otherwise it should be an explicit error
      typecheckExpr = s"${getFreshVarName(varName)}.flatMap($varName => $typecheckExpr)"

    return typecheckExpr

  def compileDestructurings(rule: RuleDecl): Seq[String] = rule.conclusion.assertion match {
    case HasType(Term.Function(_, fnArgs*), _) if fnArgs.exists(_.variables.nonEmpty) =>
      for case (Term.Function(fnName, Term.Variable(metaVarName)), idx) <- fnArgs.zipWithIndex yield
        s"val ${getFreshVarName(metaVarName)} = ${getFreshVarName("e", idx)} match { case $fnName(v) => Right(v) ; case _ => Left(\"Not a $fnName\") }"
    case _ => Seq()
  }

  def compileRule(constructor: Term, rule: RuleDecl, inductionDeclNames: PartialFunction[Judgement, String], indent: String): String =
    val Judgement(conclEnv, HasType(concl, conclTyp)) = rule.conclusion
    
    val conds = generateSyntacticConditions(constructor, concl) ++ generateEnvironmentPartConditions(conclEnv)
    
    val premises = 
      for case judg @ Judgement(_, HasType(_, premTyp)) <- rule.premises
      yield (inductionDeclNames(judg), premTyp)
    
    val extraPremises = 
      for
        case EnvironmentPart.Bindings(bindings) <- conclEnv.parts
        case (nameVarExpr, typ: Type.Variable) <- bindings.map(generateBinding)
      yield 
        (s"Right(env($nameVarExpr))", typ)
      
    val body = generateTypeCheckIf(premises ++ extraPremises, conclTyp, leaveOpen = conds.isEmpty)
    
    if conds.isEmpty then
      body.mkString("\r\n" + indent)
    else
      (Seq(
        s"if ${conds.mkString(" && ")} then",
      ) ++ body.map("  " ++ _) ++ Seq(
        s"else "
      )).mkString("\r\n" + indent)

  def generateSyntacticConditions(constructor: Term, conclTerm: Term): Iterable[String] =
    val subst = constructor.matches(conclTerm).get.filter((_, v) => v match { 
      case Term.Variable(_) => false 
      case _ => true
    })

    subst.map { (n, v) =>
      if v.variables.nonEmpty then
        // TODO: Hard-coded for a single variable for now. Probably not worth generalizing before the generated code
        // structure (and code generation strategy) is reviewed.
        val varName = v.variables.head
        s"_$varName.isRight"
      else
        s"$n == ${compile(v)}"
    }

  def generateEnvironmentPartConditions(env: Environment): Iterable[String] =
    val sizeOpt = 
      if env.parts.isEmpty || env.parts.exists(_.isInstanceOf[EnvironmentPart.Variable])
      then None
      else Some(env.parts.collect({ case EnvironmentPart.Bindings(bs) => bs.length }).sum)

    val sizeCondition = sizeOpt.map(size => 
      if size == 0 
      then "env.isEmpty"
      else s"env.size == $size"
    )

    sizeCondition.toSeq ++ env.parts.flatMap(generateEnvironmentPartConditions)

  def generateEnvironmentPartConditions(env: EnvironmentPart): Iterable[String] = env match {
    case EnvironmentPart.Bindings(bindings) =>  
      // For each binding, if we have an expected type, check right away, otherwise just check for containment
      for 
        b <- bindings
        (varNameExpr, typ) = generateBinding(b)
      yield typ match {
        case t @ Type.Named(_) => s"env.get($varNameExpr) == Some(${compileNamedType(t)})"
        case Type.Variable(_) => s"env.contains($varNameExpr)"
      }
    case EnvironmentPart.Variable(_) => 
      Seq()
  }

  def generateTypeCheckIf(premisesToCheck: Seq[(String, Type)], conclTyp: Type, leaveOpen: Boolean): Seq[String] =
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
    val (premConds, typeVarEnv) = premisesToCheck.foldLeft(Seq[String](), Map[String, String]()) {
      case ((conds, typeVarEnv), (typCheckRes, premTyp)) =>
        premTyp match {
          case t @ Type.Named(_) => 
            (conds :+ s"$typCheckRes == Right(${compileNamedType(t)})", typeVarEnv)
          
          case Type.Variable(typVarName) =>
            if typeVarEnv.contains(typVarName) then
              (conds :+ s"$typCheckRes == ${typeVarEnv(typVarName)}", typeVarEnv)
            else
              val newTypeVarEnv = typeVarEnv + (typVarName -> typCheckRes)
              // TODO: review this temporary hack used to prettify the generated code
              if typCheckRes.startsWith("Right(") then
                (conds, newTypeVarEnv)
              else
                (conds :+ s"$typCheckRes.isRight", newTypeVarEnv)
        }
    }
    
    val conclTypRes = conclTyp match {
      case t @ Type.Named(_) => s"Right(${compileNamedType(t)})"
      case Type.Variable(name) => typeVarEnv(name)
    }

    if premConds.isEmpty 
      || (premConds.length == 1 && !typeVarEnv.isEmpty) // simplification of the special case of a single "isRight" condition
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

  def compile(tsDecl: TypeSystemDecl): String =
    s"""
    import tyes.runtime.*
    import example.*
    
    object ${getTypeSystemObjectName(tsDecl)} extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case ${(for case Type.Named(tname) <- tsDecl.types yield tname.capitalize).mkString(", ")}
    
      def typecheck(exp: LExpression, env: Map[String, Type]): Either[String, Type] = exp match {
        ${compileTypecheck(tsDecl, "        ")}
        case _ => ${getTypeErrorString("exp")}
      }
    """
