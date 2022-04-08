import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.DirectoryStream
import scala.jdk.CollectionConverters.*
import scala.util.Using
import example.*
import tyes.model.*
import tyes.compiler.*

object ExampleTypeChecker extends tyes.runtime.TypeSystem[LExpression]:
  type T = Type

  enum Type:
    case Zero

  def typecheck(exp: LExpression): Either[String, Type] = exp match {
    case LNumber(_c1) =>
      if _c1 == 1 then 
        Right(Type.Zero)
      else 
        Left(s"TypeError: no type for `$exp`")
    case LPlus(e1, e2) => 
      val t1 = typecheck(e1)
      val t2 = typecheck(e2)
      if t1 == Right(Type.Zero) && t2 == Right(Type.Zero) then
        Right(Type.Zero)
      else 
        Left(s"TypeError: no type for `$exp`")
    case _ => 
      Left(s"TypeError: no type for `$exp`")
  }

@main def main: Unit =
  val expParser = LExpressionParser;
  val expTexts = List("1", "2", "3", "1 + 2", "2 + 3", "1 + 1", "3 + 5")
  val parsedExps = for expText <- expTexts yield {
    val parseRes = expParser.parse(expText)
    println(s"${expText} parses to ${parseRes}")
    parseRes
  }
  
  val exps = for case LExpressionParser.Success(exp, _) <- parsedExps yield exp
  if exps.isEmpty then
    println("No expressions")
    return
  
  import LExpressionExtensions.given
  object TsDeclParser extends TyesParser(LExpressionContextParser)

  val samplesDirPath = Paths.get("./samples/")
  val sampleFiles = Using(Files.newDirectoryStream(samplesDirPath, "*.tye"))(_.asScala.toList)
  for samplePath <- sampleFiles.get do
    val sampleSrc = Files.readString(samplePath)
    val tsDecl = TsDeclParser.parse(sampleSrc)
    println(tsDecl)

    println()
    println("### Run validator")
    val validationErrors = TyesValidator.validate(tsDecl.get)
    Console.err.println(validationErrors.mkString("\r\n"))

    if validationErrors.isEmpty then  
      println()
      println(s"### Run interpreter")
      for e <- exps do
        println(s"${e} has type ${tyes.interpreter.TyesInterpreter.typecheck(tsDecl.get, e)}")
    
      println()
      println("### Run code generation")
      val src = TyesCodeGenerator.compile(tsDecl.get)
      println(src)
      println()

      println("### Invoke generated code")
      val tsClassName = TyesCodeGenerator.getTypeSystemObjectName(tsDecl.get)
      val m = new javax.script.ScriptEngineManager(this.getClass().getClassLoader())
      val e = m.getEngineByName("scala")
      if e == null then
        println("No script engine found")
      else
        val rtTypeSystem = e.eval(src + s"\r\n${tsClassName}").asInstanceOf[tyes.runtime.TypeSystem[LExpression]]
        for e <- exps do
          println(s"${e} has type ${rtTypeSystem.typecheck(e)}")
