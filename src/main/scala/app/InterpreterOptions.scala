package app

case class InterpreterOptions(
  srcFilePath: String,
  expression: Option[String]
)

object InterpreterOptions extends OptionsParser[InterpreterOptions]:
  import ArgParsers.*

  protected override def commandName = "tyei"

  protected override def optionsSyntaxDocs = "<srcFilePath> [-e <expression>]"

  protected override def options = sourceFile ~ expressionOption.? ^^ { 
    case srcPath ~ exprOpt => InterpreterOptions(srcPath, exprOpt)
  }

  def expressionOption = "-e" ~>! anyNonFlag.+.withFailureMessage("no expression specified") ^^ {
    exprParts => exprParts.mkString(" ")
  }

  def sourceFile = anyNonFlag.withFailureMessage("no source file specified")
