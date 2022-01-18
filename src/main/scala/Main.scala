import tyes.model.*
import tyes.compiler.*

object TSSamples:
  val samples = Seq("""
    typesystem I
      rule One infers 1 : int
    """,
    """
    typesystem OneOrTwo
      rule One infers 1 : one
      rule Two infers 2 : two
    """,
    """
    typesystem Any
      rule Any infers e : any
    """,
    """
    typesystem AnyWithOverlap
      rule Any infers e : any
      rule One infers 1 : one
    """
  )

object ExampleTypeChecker extends tyes.runtime.TypeSystem[LExpression]:
  type T = Type

  enum Type:
    case Any, One

  def typecheck(exp: LExpression): Either[Type, String] = exp match {
    case e => Left(Type.Any)
    // case LNumber(num) => num match {
    //   case 1 => Left(Type.One)
    //   case _ => Right(s"TypeError: no type for `$num`")
    // }
  }

@main def main: Unit =
  val expParser = LExpressionParser;
  val expTexts = List("1", "2", "3")
  val parsedExps = for expText <- expTexts yield {
    val parseRes = expParser.parse(expText)
    println(s"${expText} parses to ${parseRes}")
    parseRes
  }
  
  val exps = for case Parsers.Success(exp, _) <- parsedExps yield exp
  
  import LExpressionExtensions.given
  object TsDeclParser extends TyesParser(expParser.toTermParser)
  for tsSource <- TSSamples.samples do
    val tsDecl = TsDeclParser.parse(tsSource)
    println(tsDecl)
    
    println()
    println(s"### Run interpreter")
    for e <- exps do
      println(s"${e} has type ${tyes.interpreter.TyesInterpreter.typecheck(tsDecl.get, e)}")
  
    println()
    println("### Run compiler")
    val src = TyesCompiler.compile(tsDecl.get)
    println(src)
    println()

    val tsClassName = TyesCompiler.getTypeSystemObjectName(tsDecl.get)
    val m = new javax.script.ScriptEngineManager(getClass().getClassLoader())
    val e = m.getEngineByName("scala")
    if e == null then
      println("No script engine found")
    else
      val rtTypeSystem = e.eval(src + s"\r\n${tsClassName}").asInstanceOf[tyes.runtime.TypeSystem[LExpression]]
      for e <- exps do
        println(s"${e} has type ${rtTypeSystem.typecheck(e)}")
