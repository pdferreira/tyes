import java.nio.file.*
import java.nio.file.DirectoryStream
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import tyes.compiler.*
import tyes.model.*

object CommandLine:
  
  @main def tyec(args: String*): Unit =
    if args.isEmpty then
      Console.err.println("Syntax: tyec <sourceFileOrDirPath> [<targetDirPath>]")
      return

    val srcFileOrDirPath = Path.of(args(0)).toAbsolutePath
    if !Files.exists(srcFileOrDirPath) then
      Console.err.println(s"File or directory not found: ${srcFileOrDirPath}")
      return

    val srcPaths = 
      if Files.isDirectory(srcFileOrDirPath) 
      then Using(Files.newDirectoryStream(srcFileOrDirPath, "*.tye"))(_.asScala.toSeq).get
      else Seq(srcFileOrDirPath)

    val dstDirPath = 
      if args.isDefinedAt(1) then Path.of(args(1)).toAbsolutePath 
      else if Files.isDirectory(srcFileOrDirPath) then srcFileOrDirPath
      else srcFileOrDirPath.getParent()

    val scalaDstDirPath = dstDirPath.resolve("src")
    val binDstDirPath = dstDirPath.resolve("bin")

    // Ensure required directories exist
    for dirPath <- Seq(dstDirPath, scalaDstDirPath, binDstDirPath) do
      Files.createDirectories(dirPath)

    for path <- srcPaths do
      println(s"Compiling '${path.toString}'")

      val srcContent = Files.readString(path) 
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