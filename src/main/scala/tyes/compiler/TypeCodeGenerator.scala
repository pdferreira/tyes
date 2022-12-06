package tyes.compiler

import tyes.model.*
import tyes.model.TyesLanguageExtensions.*

object TypeCodeGenerator:

  val typeEnumName = "Type"

  def compile(typ: Type, typSubst: Map[String, String] = Map()): String = typ match {
    case Constants.Types.any => "_"
    case Type.Named(name) => s"$typeEnumName.${name.capitalize}"
    case Type.Variable(name) => typSubst.getOrElse(name, throw new Exception(s"Unbound type variable: $name"))
    case Type.Composite(name, args*) => args.map(compile(_, typSubst)).mkString(s"$typeEnumName.${name.capitalize}(", ", ", ")")
  }

  def genSpecializationFunction(typ: Type.Composite): String =
    val name = typ.name.capitalize
    s"cast[$typeEnumName.$name]"

  def genTypeVariableGetters(typ: Type, baseGetter: String = ".getOrElse(???)"): Seq[(Type.Variable, String)] = typ match {
    case tv @ Type.Variable(_) => Seq((tv, baseGetter))
    case Type.Named(_) => Seq()
    case Type.Composite(_, args*) =>
      for 
        (t, idx) <- args.zipWithIndex
        (tv, getter) <- genTypeVariableGetters(t, baseGetter)
      yield
        (tv, s"${genArgumentTypeGetter(idx)}$getter")
  }

  def genArgumentTypeGetter(idx: Int): String = s".map(_.t${idx + 1})"

  def genTypeConditions(typeCodeStr: String, typ: Type, typeCodeEnv: Map[String, String] = Map()): (Seq[String], Map[String, String]) = typ match {
    case t @ Type.Named(_) => 
      (Seq(s"$typeCodeStr == Right(${TypeCodeGenerator.compile(t)})"), typeCodeEnv)
    
    case Type.Variable(typVarName) =>
      if typeCodeEnv.contains(typVarName) then
        (Seq(s"$typeCodeStr == ${typeCodeEnv(typVarName)}"), typeCodeEnv)
      else
        (Seq(s"$typeCodeStr.isRight"), typeCodeEnv + (typVarName -> typeCodeStr))

    case Type.Composite(name, args*) => 
      args.zipWithIndex.foldLeft((Seq[String](), typeCodeEnv)) { case ((conds, typeCodeEnv), (argTyp, idx)) =>
        val getTypeArgExpr = typeCodeStr + TypeCodeGenerator.genArgumentTypeGetter(idx)
        val (_, newTypeCodeEnv) = genTypeConditions(getTypeArgExpr, argTyp, typeCodeEnv)
        (conds :+ s"$typeCodeStr.isRight", newTypeCodeEnv)
      }
  }

  private def getTypeConstructors(types: Iterable[Type]): Set[Type] = types.flatMap({
    case tn @ Type.Named(_) => Set(tn)
    case Type.Variable(_) => Set()
    case Type.Composite(name, args*) => 
      val self = Type.Composite(name, args.indices.map(idx => Type.Variable(s"t${idx + 1}"))*)
      Set(self) ++ getTypeConstructors(args)
  }).toSet

  private def compileTypeConstructor(typ: Type): String = typ match {
    case Type.Named(name) => name.capitalize
    case Type.Composite(name, args*) =>
      val argNames = args.map(t => t.asInstanceOf[Type.Variable].name)
      name.capitalize + argNames.map(argName => s"$argName: $typeEnumName").mkString("(", ", ", ")")
        + s" extends $typeEnumName, " + argNames.mkString("tyes.runtime.CompositeType(", ", ", ")")
    case _ => throw new Exception(s"Not a type constructor: $typ")
  }

  def compile(tsDecl: TypeSystemDecl, indent: String): String =
    s"""
    enum $typeEnumName extends tyes.runtime.Type:
      ${getTypeConstructors(tsDecl.types).toSeq.sortBy({
        case Type.Named(name) => (0, name)
        case Type.Variable(_) => (0, "")
        case Type.Composite(name, args*) => (args.length, name)
      }).map(compileTypeConstructor).mkString("case ", s"\r\n      case ", "")}
    """
      .stripPrefix("\r\n")
      .stripSuffix("\r\n    ")
      .stripIndent()
      .linesIterator
      .mkString("\r\n" + indent)
