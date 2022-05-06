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

  def tyei(args: String*): Unit =
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
    for tsDecl <- parseTypeSystem(srcContent) do
      options.expression match {
        case Some(expSrc) =>
          invokeInterpreter(tsDecl, expSrc)
        case None =>
          val lineIt = Iterator.continually { StdIn.readLine("> ") }
          for line <- lineIt.takeWhile(_ != null) do
            if !line.isBlank then
              invokeInterpreter(tsDecl, line)
      }

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

  private def invokeInterpreter(tsDecl: TypeSystemDecl, expSrc: String): Unit =
      for exp <- parseLExpression(expSrc) do
        TyesInterpreter.typecheck(tsDecl, exp) match {
          case Some(Type.Named(typName)) => 
            println(typName)
          case _ => 
            Console.err.println("No type for expression")
        }
      