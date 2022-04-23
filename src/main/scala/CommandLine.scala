import java.nio.file.*
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import tyes.compiler.*
import tyes.model.*
import scala.util.Try

object CommandLine:
  
  @main def tyec(args: String*): Unit =
    if args.isEmpty then
      Console.err.println("Syntax: tyec [-out <targetDirPath>] <srcFilePaths...>")
      return

    val targetDirPathOpt = if args(0) == "-out" then Some(args(1)) else None

    val srcPathArgs = if targetDirPathOpt.isDefined then args.drop(2) else args
    val srcPaths = Try { srcPathArgs.map(Paths.get(_)) }
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
      val dstDirPath = targetDirPathOpt.map { p =>
        Path.of(p).toAbsolutePath
      } getOrElse {
        path.getParent()
      }

      val scalaDstDirPath = dstDirPath.resolve("src")
      val binDstDirPath = dstDirPath.resolve("bin")

      for dirPath <- Seq(scalaDstDirPath, binDstDirPath) do
        Files.createDirectories(dirPath)

      invokeCompiler(path, scalaDstDirPath, binDstDirPath)

  private def invokeCompiler(srcPath: Path, scalaDstDirPath: Path, binDstDirPath: Path): Unit = 
    val srcContent = Files.readString(srcPath) 
    val tsDecl = LExpressionTyesParser.parse(srcContent)

    val validationErrors = TyesValidator.validate(tsDecl.get)
    if validationErrors.isEmpty then
      println(s"\tGenerating scala sources...")
      val generatedCode = TyesCodeGenerator.compile(tsDecl.get)

      val dstFileName = s"${TyesCodeGenerator.getTypeSystemObjectName(tsDecl.get)}.scala"
      val scalaDstFilePath = scalaDstDirPath.resolve(dstFileName)
      Files.write(scalaDstFilePath, generatedCode.getBytes)

      println(s"\tGenerating scala binaries...")
      val driver = new dotty.tools.dotc.Driver()
      val report = driver.process(s"-usejavacp -d ${binDstDirPath} ${scalaDstFilePath}".split(" "))
      if report.hasErrors then
        println(report.summary)
    else
      Console.err.println(validationErrors.mkString("\r\n"))
