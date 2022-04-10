package tyes.compiler

import tyes.model.*

object TyesCodeGenerator:

  def getAllTypes[E](asrt: Assertion): Set[Type] = Set(asrt match {
    case HasType(_, typ) => typ
  })

  def getAllTypes[E](tsDecl: TypeSystemDecl): Set[Type] =
    (for 
      case RuleDecl(_, prems, concl) <- tsDecl.rules
      a <- concl +: prems
      t <- getAllTypes(a)
    yield t).toSet 

  def getTypeSystemObjectName[E](tsDecl: TypeSystemDecl): String = tsDecl.name.getOrElse("") + "TypeSystem"

  def compile(term: Term): String = term match {
    case Term.Constant(value) => value.toString
    case Term.Variable(name) => name
    case Term.Function(name, args*) => name + args.map(compile).mkString("(", ", ", ")")
  }

  def compileNamedType(typ: Type.Named): String = s"Type.${typ.name.capitalize}"

  def getFreshVarName(base: String, index: Int): String = s"_${base}${index + 1}"

  def compileTypecheckV2(tsDecl: TypeSystemDecl, indent: String): String =
    val rulesByConstructor = 
      (for
        r <- tsDecl.rules
        case HasType(term, _) <- Seq(r.conclusion)
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
          case Seq(r @ RuleDecl(_, Seq(), HasType(Term.Variable(_), _))) => compileRule(c, r, Map(), indent)
          case _ =>
            val premiseTerms = 
              for case (HasType(prem, premTyp), idx) <- rs.flatMap(_.premises).zipWithIndex
              yield (prem, getFreshVarName("t", idx), premTyp)

            val inductionDeclNames = premiseTerms.groupMap(_._1)(_._2).mapValues(_.head)
            val inductionDecls = (
              for (premTerm, typVarName) <- inductionDeclNames.toSeq.sortBy(_._2) 
              yield s"\r\n${indent}  val ${typVarName} = typecheck(${compile(premTerm)})" 
            ).mkString

            val defaultCase = s"\r\n${indent}    ${getTypeErrorString("exp")}"
            val ruleEvaluations = rs.foldRight(defaultCase)((r, res) => compileRule(c, r, inductionDeclNames, indent + "  ") + " " + res)
            s"${inductionDecls}\r\n${indent}  ${ruleEvaluations}"
        }
        s"case ${compile(c)} => ${caseBody}" 
    ).mkString(s"\r\n${indent}")

  def getTypeErrorString(expVarName: String): String = s"Left(s\"TypeError: no type for `$$${expVarName}`\")"

  def compileRule(constructor: Term, rule: RuleDecl, inductionDeclNames: PartialFunction[Term, String], indent: String): String =
    val HasType(concl, typ) = rule.conclusion
    val subst = constructor.matches(concl).get
    val subst2 = subst.filter((_, v) => v match { 
      case Term.Variable(_) => false 
      case _ => true
    })
    val premises = 
      for case HasType(prem, premTyp) <- rule.premises
      yield (inductionDeclNames(prem), premTyp)
    
    val body = generateTypeCheckIf(premises, typ, leaveOpen = subst2.isEmpty)

    if subst2.isEmpty then
      body.mkString("\r\n" + indent)
    else
      (Seq(
        s"if ${subst2.map((n, v) => s"${n} == ${compile(v)}").mkString(" && ")} then",
      ) ++ body.map("  " ++ _) ++ Seq(
        s"else "
      )).mkString("\r\n" + indent)

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
            (conds :+ s"${typCheckRes} == Right(${compileNamedType(t)})", typeVarEnv)
          
          case Type.Variable(typVarName) =>
            if typeVarEnv.contains(typVarName) then
              (conds :+ s"${typCheckRes} == ${typeVarEnv(typVarName)}", typeVarEnv)
            else
              val newTypeVarEnv = typeVarEnv + (typVarName -> typCheckRes)
              (conds :+ s"${typCheckRes}.isRight", newTypeVarEnv)
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
        s"  ${conclTypRes}"
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
        case ${(for case Type.Named(tname) <- getAllTypes(tsDecl) yield tname.capitalize).mkString(", ")}
    
      def typecheck(exp: LExpression): Either[String, Type] = exp match {
        ${compileTypecheckV2(tsDecl, "        ")}
        case _ => ${getTypeErrorString("exp")}
      }
    """
