package tyes.cli

case class CompilerOptions(
  srcFilePaths: Seq[String],
  targetDirPath: Option[String],
  versionId: Option[String],
  skipValidation: Boolean = false
)

object CompilerOptions extends OptionsParser[CompilerOptions]:
  import ArgParsers.*

  protected override def commandName = "tyec"

  protected override def optionsSyntaxDocs = "[-out <targetDirPath>] [-skipValidation] [-version <versionId>] <srcFilePaths...>"
  
  protected override def options = outputDirOption.? ~ skipValidationOption ~ versionOption.? ~ sourceFiles ^^ {
    case targetPath ~ skipValidation ~ versionId ~ srcPaths => CompilerOptions(srcPaths, targetPath, versionId, skipValidation)
  }

  private def outputDirOption = "-out" ~>! anyNonFlag.withFailureMessage("no output directory specified")

  private def versionOption = "-version" ~>! anyNonFlag.withFailureMessage("no version specified")

  private def sourceFiles = anyNonFlag.+.withFailureMessage("no source file(s) specified")

  private def skipValidationOption = "-skipValidation".? ^^ {
    case None => false
    case Some(_) => true
  }
