package tyes.cli

case class InterpreterOptions(
  srcFilePath: String,
  versionId: Option[String],
  expression: Option[String],
  skipValidation: Boolean = false
)

object InterpreterOptions extends OptionsParser[InterpreterOptions]:
  import ArgParsers.*

  protected override def commandName = "tyer"

  protected override def optionsSyntaxDocs = "<srcFilePath> [-skipValidation] [-version <versionId>] [-e <expression>]"

  protected override def options = sourceFile ~ skipValidationOption ~ versionOption.? ~ expressionOption.? ^^ { 
    case srcPath ~ skipValidation ~ versionOpt ~ exprOpt => InterpreterOptions(srcPath, versionOpt, exprOpt, skipValidation)
  }

  private def expressionOption = "-e" ~>! anyNonFlag.+.withFailureMessage("no expression specified") ^^ {
    exprParts => exprParts.mkString(" ")
  }

  private def sourceFile = anyNonFlag.withFailureMessage("no source file specified")

  private def versionOption = "-version" ~>! anyNonFlag.withFailureMessage("no version specified")

  private def skipValidationOption = "-skipValidation".? ^^ {
    case None => false
    case Some(_) => true
  }
