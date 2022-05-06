package tyes.cli

case class CompilerOptions(
  srcFilePaths: Seq[String],
  targetDirPath: Option[String]
)

object CompilerOptions extends OptionsParser[CompilerOptions]:
  import ArgParsers.*

  protected override def commandName = "tyec"

  protected override def optionsSyntaxDocs = "[-out <targetDirPath>] <srcFilePaths...>"
  
  protected override def options = outputDirOption.? ~ sourceFiles ^^ {
    case targetPath ~ srcPaths => CompilerOptions(srcPaths, targetPath)
  }

  private def outputDirOption = "-out" ~>! anyNonFlag.withFailureMessage("no output directory specified")

  private def sourceFiles = anyNonFlag.+.withFailureMessage("no source file(s) specified")
