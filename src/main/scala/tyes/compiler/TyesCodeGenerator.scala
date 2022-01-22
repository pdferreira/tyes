package tyes.compiler

import tyes.model.*

object TyesCodeGenerator:

  def getAllTypes[E](asrt: Assertion): Set[Type] = Set(asrt match {
    case HasType(_, typ) => typ
  })

  def getAllTypes[E](tsDecl: TypeSystemDecl): Set[Type] =
    (for 
      case RuleDecl(_, concl) <- tsDecl.rules
      t <- getAllTypes(concl)
    yield t).toSet 

  def getTypeSystemObjectName[E](tsDecl: TypeSystemDecl): String = tsDecl.name.getOrElse("") + "TypeSystem"

  def compile(term: Term): String = term match {
    case Term.Constant(value) => value.toString
    case Term.Variable(name) => name
    case Term.Function(name, args*) => name + args.map(compile).mkString("(", ", ", ")")
  }

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
          for case RuleDecl(_, HasType(term, Type.Named(tname))) <- tsDecl.rules
          yield s"case ${compile(term)} => Right(Type.${tname.capitalize})"
        ).mkString("\r\n        ")}
        case _ => Left(s"TypeError: no type for `$$exp`")
      }
    """
