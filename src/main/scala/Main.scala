import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.DirectoryStream
import scala.jdk.CollectionConverters.*
import scala.util.Using
import example.*
import tyes.model.*
import tyes.compiler.*
import tyes.interpreter.*
import tyes.cli.*

object ExampleTypeChecker extends tyes.runtime.old.TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One, Two

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(_c1) =>
      if _c1 == 1 then 
        Right(Type.One)
      else if _c1 == 2 then
        Right(Type.Two)
      else 
        Left(s"TypeError: no type for `$exp`")
    case LPlus(e1, e2) => 
      val _t1 = typecheck(e1, env)
      val _t2 = typecheck(e2, _t1.map(t => env + ("pi" -> t)).getOrElse(env))
      if _t1.isRight && _t2.isRight && _t1 == _t2 then
        _t1
      else
        Left(s"TypeError: no type for `$exp`")
    case _ => 
      Left(s"TypeError: no type for `$exp`")
  }

@main def tyec(args: String*): Unit = CommandLine.tyec(args*)

@main def tyer(args: String*): Unit = CommandLine.tyer(args*)

@main def main: Unit =
  val expParser = LExpressionParser();
  val expTexts = List("1", "2", "3", "1 + 2", "2 + 3", "1 + 1", "3 + 5", "2 + 1", "2 + 2")
  val parsedExps = for expText <- expTexts yield {
    val parseRes = expParser.parse(expText)
    println(s"$expText parses to $parseRes")
    parseRes
  }
  
  val exps = for case expParser.Success(exp, _) <- parsedExps yield exp
  if exps.isEmpty then
    println("No expressions")
    return
  
  val samplesDirPath = Paths.get("./samples/")
  val sampleFiles = Using(Files.newDirectoryStream(samplesDirPath, "*.tye"))(_.asScala.toList)
  for samplePath <- sampleFiles.get do
    val sampleSrc = Files.readString(samplePath)
    val tsDecl = LExpressionTyesParser.parse(sampleSrc)
    println(tsDecl)

    println()
    println("### Run validator")
    val validationErrors = TyesValidator.validate(tsDecl.get)
    Console.err.println(validationErrors.mkString("\r\n"))

    if validationErrors.isEmpty then  
      println()
      println(s"### Run interpreter")
      
      import LExpressionExtensions.given
      val interpreterResults = exps.map(e => TyesInterpreter.typecheck(tsDecl.get, e))
      for (e, idx) <- exps.zipWithIndex do
        println(s"$e has type ${interpreterResults(idx)}")
