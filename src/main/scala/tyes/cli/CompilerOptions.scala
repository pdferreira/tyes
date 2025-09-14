package tyes.cli

case class CompilerOptions(
  srcFilePaths: Seq[String],
  targetDirPath: Option[String],
  skipValidation: Boolean = false
)

object CompilerOptions extends OptionsParser[CompilerOptions]:
  import ArgParsers.*

  protected override def commandName = "tyec"

  protected override def optionsSyntaxDocs = "[-out <targetDirPath>] [-skipValidation] <srcFilePaths...>"
  
  protected override def options = outputDirOption.? ~ skipValidationOption ~ sourceFiles ^^ {
    case targetPath ~ skipValidation ~ srcPaths => CompilerOptions(srcPaths, targetPath, skipValidation)
  }

  private def outputDirOption = "-out" ~>! anyNonFlag.withFailureMessage("no output directory specified")

  private def sourceFiles = anyNonFlag.+.withFailureMessage("no source file(s) specified")

  private def skipValidationOption = "-skipValidation".? ^^ {
    case None => false
    case Some(_) => true
  }
