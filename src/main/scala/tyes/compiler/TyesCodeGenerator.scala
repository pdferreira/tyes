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

  def compile(tsDecl: TypeSystemDecl): String =
    s"""
    import tyes.runtime.*
    import example.*

    object ${getTypeSystemObjectName(tsDecl)} extends TypeSystem[LExpression]:
      type T = Type

      enum Type:
        case ${(for case Type.Named(tname) <- getAllTypes(tsDecl) yield tname.capitalize).mkString(", ")}

      def typecheck(exp: LExpression): Either[String, Type] = exp match {
        ${(
          // TODO: detect at compile-time whether there's any need to try the next case
          // so that the generated code is cleaner, instead of using the match guards
          for case RuleDecl(_, prems, HasType(term, typ)) <- tsDecl.rules
          yield s"case ${compile(term) + compilePremises(prems)} => Right(${compile(typ)})"
        ).mkString("\r\n        ")}
        case _ => Left(s"TypeError: no type for `$$exp`")
      }
    """
