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

  def compile(typ: Type): String = typ match {
    case Type.Named(name) => s"Type.${name.capitalize}"
  }

  def compilePremises(prems: Seq[Assertion]): String =
    if prems.isEmpty then
      ""
    else
      val premStrs = prems.map { case HasType(term, typ) => s"typecheck(${compile(term)}) == Right(${compile(typ)})"}
      premStrs.mkString(" if ", " && ", "")

  def compileTypecheckV1(tsDecl: TypeSystemDecl, indent: String): String =
    (
      // TODO: detect at compile-time whether there's any need to try the next case
      // so that the generated code is cleaner, instead of using the match guards
      for case RuleDecl(_, prems, HasType(term, typ)) <- tsDecl.rules
      yield s"case ${compile(term) + compilePremises(prems)} => Right(${compile(typ)})"
    ).mkString(s"\r\n${indent}")

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
          case Seq(r @ RuleDecl(_, Seq(), HasType(Term.Variable(_), _))) => compile(c, r, Map(), indent)
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
            val ruleEvaluations = rs.foldRight(defaultCase)((r, res) => compile(c, r, inductionDeclNames, indent + "  ") + " " + res)
            s"${inductionDecls}\r\n${indent}  ${ruleEvaluations}"
        }
        s"case ${compile(c)} => ${caseBody}" 
    ).mkString(s"\r\n${indent}")

  def getTypeErrorString(expVarName: String): String = s"Left(s\"TypeError: no type for `$$${expVarName}`\")"

  def compile(constructor: Term, rule: RuleDecl, inductionDeclNames: PartialFunction[Term, String], indent: String): String =
    val HasType(concl, typ) = rule.conclusion
    val subst = constructor.matches(concl).get
    val subst2 = subst.filter((_, v) => v match { 
      case Term.Variable(_) => false 
      case _ => true
    })
    val premises = 
      for case HasType(prem, premTyp) <- rule.premises
      yield (inductionDeclNames(prem), premTyp)
    
    val body = 
      if premises.isEmpty then 
        Seq(s"Right(${compile(typ)})")
      else
        Seq(
          (
            for (typVarName, premTyp) <- premises
            yield s"${typVarName} == Right(${compile(premTyp)})" 
          ).mkString("if ", " && ", " then"),
          s"  Right(${compile(typ)})",
          "else"
        )

    if subst2.isEmpty then
      body.mkString("\r\n" + indent)
    else
      (Seq(
        s"if ${subst2.map((n, v) => s"${n} == ${compile(v)}").mkString(" && ")} then",
      ) ++ body.map("  " ++ _) ++ Seq(
        s"else "
      )).mkString("\r\n" + indent)

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
