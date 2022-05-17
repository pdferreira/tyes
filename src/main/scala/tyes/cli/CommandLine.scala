package tyes.cli

import java.nio.file.*
import scala.io.Source
import scala.io.StdIn
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.Try
import scala.util.parsing.combinator.Parsers
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import tyes.compiler.*
import tyes.interpreter.*
import tyes.model.*
import example.*
import utils.parsers.ParserExtensions
import utils.parsers.ParserExtensions.*
import LExpressionExtensions.given

object CommandLine:

  def tyec(args: String*): Unit =
    val options = CompilerOptions.parse(args) match {
      case Left(message) => 
        Console.err.println(message)
        return
      case Right(opts) => opts
    }

    val srcPaths = Try { options.srcFilePaths.map(Paths.get(_)) }
    if srcPaths.isFailure then
      Console.err.println(srcPaths.failed.get.getMessage)
      return

    for path <- srcPaths.get do
      if Files.notExists(path) then
        Console.err.println(s"File not found: ${path.toString}")
        return

    for path <- srcPaths.get do
      println(s"Compiling '${path.toString}'")

      // Get output dir and ensure required directories exist
      val dstDirPath = options.targetDirPath.map { p =>
        Path.of(p).toAbsolutePath
      } getOrElse {
        path.getParent()
      }

      val scalaDstDirPath = dstDirPath.resolve("src")
      val binDstDirPath = dstDirPath.resolve("bin")

      for dirPath <- Seq(scalaDstDirPath, binDstDirPath) do
        Files.createDirectories(dirPath)

      invokeCompiler(path, scalaDstDirPath, binDstDirPath)

  def tyer(args: String*): Unit =
    val options = InterpreterOptions.parse(args) match {
      case Left(message) => 
        Console.err.println(message)
        return
      case Right(opts) => opts
    }

    val srcPath = Try { Paths.get(options.srcFilePath) }
    if srcPath.isFailure then
      Console.err.println(srcPath.failed.get.getMessage)
      return

    if Files.notExists(srcPath.get) || Files.isDirectory(srcPath.get) then
      Console.err.println(s"File not found: ${srcPath.get.toString}")
      return

    val srcContent = Files.readString(srcPath.get)
    val fileName = srcPath.get.getFileName.toString
    val (nameWithoutExt, ext) = fileName.splitAt(fileName.lastIndexOf('.'))
    if ext == ".tye" then
      invokeInterpreter(srcContent, options.expression)
    else if ext == ".scala" then
      invokeRunner(nameWithoutExt, srcContent, options.expression)
    else
      Console.err.println(s"File type not recognized: ${ext}")   

  private def parseLExpression(srcContent: String): Option[LExpression] =
    LExpressionParser.parse(srcContent)
      .toOption { ns => Console.err.println(ns.msg) }

  private def parseTypeSystem(srcContent: String): Option[TypeSystemDecl] =
    LExpressionTyesParser.parse(srcContent)
      .toOption { ns => Console.err.println(ns.msg) }
      .flatMap { tsDecl =>
        val validationErrors = TyesValidator.validate(tsDecl)
        if validationErrors.isEmpty then
          Some(tsDecl)
        else
          Console.err.println(validationErrors.mkString("\r\n"))
          None
      }

  private def invokeCompiler(srcPath: Path, scalaDstDirPath: Path, binDstDirPath: Path): Unit = 
    val srcContent = Files.readString(srcPath)
    
    for tsDecl <- parseTypeSystem(srcContent) do
      println(s"\tGenerating scala sources...")
      val generatedCode = TyesCodeGenerator.compile(tsDecl)

      val dstFileName = s"${TyesCodeGenerator.getTypeSystemObjectName(tsDecl)}.scala"
      val scalaDstFilePath = scalaDstDirPath.resolve(dstFileName)
      Files.write(scalaDstFilePath, generatedCode.getBytes)

      println(s"\tGenerating scala binaries...")
      val driver = new dotty.tools.dotc.Driver()
      val report = driver.process(s"-usejavacp -d ${binDstDirPath} ${scalaDstFilePath}".split(" "))
      if report.hasErrors then
        println(report.summary)

  private def invokeInterpreter(tyesSrc: String, expSrcOption: Option[String]): Unit =
    for tsDecl <- parseTypeSystem(tyesSrc) do
      runInteractive(line => {
        for exp <- parseLExpression(line) do
          TyesInterpreter.typecheck(tsDecl, exp) match {
            case Some(Type.Named(typName)) => 
              println(typName)
            case _ => 
              Console.err.println("No type for expression")
          }
        },
        expSrcOption)

  private  def invokeRunner(objName: String, srcContent: String, expSrcOption: Option[String]): Unit =
    val engineManager = new javax.script.ScriptEngineManager(this.getClass().getClassLoader())
    val engine = engineManager.getEngineByName("scala")
    if engine == null then
      Console.err.println("Unable to load scala file, no script engine found")
    else
      val tsClassName = objName
      val rtTypeSystem = engine.eval(srcContent + s"\r\n${tsClassName}").asInstanceOf[tyes.runtime.TypeSystem[LExpression]]
      runInteractive(
        line => {
          for exp <- parseLExpression(line) do 
            rtTypeSystem.typecheck(exp, Map()) match {
              case Right(typ) => println(typ)
              case Left(errMsg) => Console.err.println(errMsg)
            }
        }, 
        expSrcOption)

  private def runInteractive(processLine: String => Unit, singleInputOpt: Option[String]): Unit =
    singleInputOpt match {
      case Some(line) =>
        processLine(line)
      case None =>
        val lineIt = Iterator.continually { StdIn.readLine("> ") }
        for line <- lineIt.takeWhile(_ != null) do
          if !line.isBlank then
            processLine(line)
    }
      