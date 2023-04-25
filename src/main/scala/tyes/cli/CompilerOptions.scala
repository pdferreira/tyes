package tyes.cli

case class CompilerOptions(
  srcFilePaths: Seq[String],
  targetDirPath: Option[String],
  versionId: Option[String]
)

object CompilerOptions extends OptionsParser[CompilerOptions]:
  import ArgParsers.*

  protected override def commandName = "tyec"

  protected override def optionsSyntaxDocs = "[-out <targetDirPath>] [-version <versionId>] <srcFilePaths...>"
  
  protected override def options = outputDirOption.? ~ versionOption.? ~ sourceFiles ^^ {
    case targetPath ~ versionId ~ srcPaths => CompilerOptions(srcPaths, targetPath, versionId)
  }

  private def outputDirOption = "-out" ~>! anyNonFlag.withFailureMessage("no output directory specified")

  private def versionOption = "-version" ~>! anyNonFlag.withFailureMessage("no version specified")

  private def sourceFiles = anyNonFlag.+.withFailureMessage("no source file(s) specified")
