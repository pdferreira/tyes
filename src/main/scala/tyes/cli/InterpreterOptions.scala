package tyes.cli

case class InterpreterOptions(
  srcFilePath: String,
  versionId: Option[String],
  expression: Option[String]
)

object InterpreterOptions extends OptionsParser[InterpreterOptions]:
  import ArgParsers.*

  protected override def commandName = "tyer"

  protected override def optionsSyntaxDocs = "<srcFilePath> [-version <versionId>] [-e <expression>]"

  protected override def options = sourceFile ~ versionOption.? ~ expressionOption.? ^^ { 
    case srcPath ~ versionOpt ~ exprOpt => InterpreterOptions(srcPath, versionOpt, exprOpt)
  }

  private def expressionOption = "-e" ~>! anyNonFlag.+.withFailureMessage("no expression specified") ^^ {
    exprParts => exprParts.mkString(" ")
  }

  private def sourceFile = anyNonFlag.withFailureMessage("no source file specified")

  private def versionOption = "-version" ~>! anyNonFlag.withFailureMessage("no version specified")
