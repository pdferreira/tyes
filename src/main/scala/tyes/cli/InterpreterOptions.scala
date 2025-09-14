package tyes.cli

case class InterpreterOptions(
  srcFilePath: String,
  expression: Option[String],
  skipValidation: Boolean = false
)

object InterpreterOptions extends OptionsParser[InterpreterOptions]:
  import ArgParsers.*

  protected override def commandName = "tyer"

  protected override def optionsSyntaxDocs = "<srcFilePath> [-skipValidation] [-e <expression>]"

  protected override def options = sourceFile ~ skipValidationOption ~ expressionOption.? ^^ { 
    case srcPath ~ skipValidation ~ exprOpt => InterpreterOptions(srcPath, exprOpt, skipValidation)
  }

  private def expressionOption = "-e" ~>! anyNonFlag.+.withFailureMessage("no expression specified") ^^ {
    exprParts => exprParts.mkString(" ")
  }

  private def sourceFile = anyNonFlag.withFailureMessage("no source file specified")

  private def skipValidationOption = "-skipValidation".? ^^ {
    case None => false
    case Some(_) => true
  }
