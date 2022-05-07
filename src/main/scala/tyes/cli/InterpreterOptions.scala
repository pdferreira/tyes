package tyes.cli

case class InterpreterOptions(
  srcFilePath: String,
  expression: Option[String]
)

object InterpreterOptions extends OptionsParser[InterpreterOptions]:
  import ArgParsers.*

  protected override def commandName = "tyer"

  protected override def optionsSyntaxDocs = "<srcFilePath> [-e <expression>]"

  protected override def options = sourceFile ~ expressionOption.? ^^ { 
    case srcPath ~ exprOpt => InterpreterOptions(srcPath, exprOpt)
  }

  private def expressionOption = "-e" ~>! anyNonFlag.+.withFailureMessage("no expression specified") ^^ {
    exprParts => exprParts.mkString(" ")
  }

  private def sourceFile = anyNonFlag.withFailureMessage("no source file specified")
