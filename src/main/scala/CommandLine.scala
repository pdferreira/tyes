import java.nio.file.*
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.Try
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.*
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import tyes.compiler.*
import tyes.interpreter.*
import tyes.model.*
import example.*
import LExpressionExtensions.given

object CommandLine:

  case class CompilerOptions(
    srcFilePaths: Seq[String] = Seq(),
    targetDirPath: Option[String] = None
  )

  class SeqPosition[T](seq: Seq[T], override val column: Int) extends Position:
    override def line: Int = 1
    override def lineContents: String = throw new NotImplementedError()
    override def longString: String =
      val sep = " "
      val elemsWithPositions = seq.map(_.toString).foldLeft(Seq[(String, Int)]()) { 
        case (acc, text) => acc match {
          case Seq() => Seq(text -> 0)
          case _ :+ (prevText, prevPos) => acc :+ (text, prevPos + prevText.length + sep.length)
        }
      }
      val line = elemsWithPositions.map(_._1).mkString(" ")
      val arrowPos = 
        if elemsWithPositions.isEmpty 
        then 0 
        else elemsWithPositions(column - 1)._2

      line + "\r\n" + " ".repeat(arrowPos) + "^"

  class SeqReader[T](seq: Seq[T], offset: Int = 0) extends Reader[T]:
    override def first: T = seq(offset)
    override def rest: Reader[T] =
      if atEnd 
      then this
      else new SeqReader(seq, offset + 1)
    override def atEnd: Boolean = offset >= seq.length
    override def pos: Position = new SeqPosition(seq, offset + 1)

  trait SeqParsers[T] extends Parsers:
    override type Elem = T
  
  object CompilerOptions:
    object Parsers extends SeqParsers[String]:
      def ensure[T](parser: Parser[T], cond: T => Boolean, errorMessage: String = "unexpected input state"): Parser[T] =
        Parser(in =>
          parser(in) match {
            case s: Success[T] => 
              if cond(s.result) 
              then s 
              else Error(errorMessage, in)
            case other => other 
          }
        )

    import Parsers.*

    def any = Parser[String] { in => 
      if in.atEnd
      then Failure("end of input", in)
      else Success(in.first, in.rest)
    }

    def anyNonFlag = ensure(any, !_.startsWith("-"), "flag in unexpected position")

    def outputDirOption(currOptions: CompilerOptions, targetIsParsed: Boolean) = 
      ensure("-out", _ => !targetIsParsed, "-out specified twice") ~>! 
      anyNonFlag.withFailureMessage("no output directory specified") ~ 
      options(currOptions, targetIsParsed = true) ^^ { 
        case path ~ opts => opts.copy(targetDirPath = Some(path)) 
      }

    def sourceFiles(currOptions: CompilerOptions) =
      anyNonFlag.+.withFailureMessage("no source file(s) specified") ^^ {
        case paths => currOptions.copy(srcFilePaths = paths)
      }

    def options(
      currOptions: CompilerOptions = CompilerOptions(),
      targetIsParsed: Boolean = false
    ): Parser[CompilerOptions] =
      outputDirOption(currOptions, targetIsParsed)
      | sourceFiles(currOptions)

    def parse(args: Seq[String]): Option[CompilerOptions] = 
      if args.isEmpty then
        Console.err.println("Syntax: tyec [-out <targetDirPath>] <srcFilePaths...>")
        return None

      (phrase(options())(new SeqReader(args)): @unchecked) match {
        case Parsers.NoSuccess(msg, next) => 
          val cmdPrefix = "tyec "
          val lineIndent = " ".repeat(cmdPrefix.length)
          val errorLocation = cmdPrefix + next.pos.longString.linesIterator.mkString("\r\n" + lineIndent)
          val middlePos = next.pos.column + cmdPrefix.length
          val msgIndent = " ".repeat(Math.max(0, middlePos - msg.length / 2))
          Console.err.println(s"${errorLocation}\r\n${msgIndent}${msg}")
          None
        case Parsers.Success(res, _) => Some(res)
      }

  @main def tyec(args: String*): Unit =
    val options = CompilerOptions.parse(args) match {
      case None => return
      case Some(opts) => opts
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

  @main def tyei(args: String*): Unit =
    if args.isEmpty then
      Console.err.println("Syntax: tyei <srcFilePath> [-e <expression>]")

    val srcPath = Try { Paths.get(args(0)) }
    if srcPath.isFailure then
      Console.err.println(srcPath.failed.get.getMessage)
      return

    if Files.notExists(srcPath.get) || Files.isDirectory(srcPath.get) then
      Console.err.println(s"File not found: ${srcPath.get.toString}")
      return

    val srcContent = Files.readString(srcPath.get)
    for tsDecl <- parseTypeSystem(srcContent) do
      if args.length > 2 && args(1) == "-e" then
        // Consume everything after as if it were a single expression
        val expSrc = args.drop(2).mkString(" ")
        for exp <- parseLExpression(expSrc) do
          TyesInterpreter.typecheck(tsDecl, exp) match {
            case Some(Type.Named(typName)) => 
              println(typName)
            case _ => 
              Console.err.println("No type for expression")
          }
      else
        // TODO: repl
        ()

  private def parseLExpression(srcContent: String): Option[LExpression] =
    (LExpressionParser.parse(srcContent): @unchecked) match {
      case LExpressionParser.Success(exp, _) =>
        Some(exp)
      case LExpressionParser.NoSuccess(message, _) =>
        Console.err.println(message)
        None
    }

  private def parseTypeSystem(srcContent: String): Option[TypeSystemDecl] =
    (LExpressionTyesParser.parse(srcContent): @unchecked) match {
      case Parsers.Success(tsDecl, _) =>
        val validationErrors = TyesValidator.validate(tsDecl)
        if validationErrors.isEmpty then
          Some(tsDecl)
        else
          Console.err.println(validationErrors.mkString("\r\n"))
          None
      case Parsers.NoSuccess(message, _) =>
        Console.err.println(message)
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
