import java.nio.file.*
import java.nio.file.DirectoryStream
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
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
      return;

    val srcPaths = 
      if Files.isDirectory(srcFileOrDirPath) 
      then Using(Files.newDirectoryStream(srcFileOrDirPath, "*.tye"))(_.asScala.toSeq).get
      else Seq(srcFileOrDirPath)

    val dstDirPath = 
      if args.isDefinedAt(1) then Path.of(args(1)).toAbsolutePath 
      else if Files.isDirectory(srcFileOrDirPath) then srcFileOrDirPath
      else srcFileOrDirPath.getParent()

    if !Files.exists(dstDirPath) then
      Files.createDirectories(dstDirPath)

    if !Files.isDirectory(dstDirPath) then
      Console.err.println(s"Not a directory: ${dstDirPath}")
      return

    for path <- srcPaths do
      println(s"Compiling '${path.toString}'")

      val srcContent = Files.readString(path) 
      val tsDecl = LExpressionTyesParser.parse(srcContent)

      val validationErrors = TyesValidator.validate(tsDecl.get)
      if validationErrors.isEmpty then
        val generatedCode = TyesCodeGenerator.compile(tsDecl.get)
        val (srcName, _) = getFileNameInfo(path.getFileName.toString)

        val dstFileName = s"${srcName}.scala"
        val dstFile = dstDirPath.resolve(dstFileName)
        println(s"Generating '${dstFile.toString}'")
        Files.write(dstFile, generatedCode.getBytes)
      else
        Console.err.println(validationErrors.mkString("\r\n"))

  private def getFileNameInfo(fileName: String): (String, String) =
    val lastPeriodIdx = fileName.lastIndexOf(".")
    fileName.splitAt(lastPeriodIdx)