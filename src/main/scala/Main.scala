import tyes.model.*
import tyes.compiler.*
import example.*

object TSSamples:
  val samples = Seq(
    """
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
    typesystem PlusOne
      rule One infers 1 : one
      rule PlusOne infers e + 1 : sumOne
    """,
    """
    typesystem Overlap
      rule OnePlus infers 1 + e : oneP
      rule TwoPlus infers 2 + e : twoP
      rule RestPlus infers e1 + e2 : otherP
    """,
    """
    typesystem PlusOneOrTwoConditional
      rule One infers 1 : one
      rule OneSum infers e1 + e2 : one
        if  e1 : one
        and e2 : one
      rule Two infers 2 : two
      rule PlusTwo infers e1 + e2 : twoPlus
        if e1 : two
    """
  )

object ExampleTypeChecker extends tyes.runtime.TypeSystem[LExpression]:
  type T = Type

  enum Type:
    case One, SumOne

  def typecheck(exp: LExpression): Either[String, Type] = exp match {
    case LNumber(1) => Right(Type.One)
    case LPlus(e1, LNumber(1)) => Right(Type.SumOne)
    case _ => Left(s"TypeError: no type for `$exp`")
  }

@main def main: Unit =
  val expParser = LExpressionParser;
  val expTexts = List("1", "2", "3", "1 + 2", "2 + 3", "1 + 1", "3 + 5")
  val parsedExps = for expText <- expTexts yield {
    val parseRes = expParser.parse(expText)
    println(s"${expText} parses to ${parseRes}")
    parseRes
  }
  
  val exps = for case Parsers.Success(exp, _) <- parsedExps yield exp
  
  import LExpressionExtensions.given
  object TsDeclParser extends TyesParser(LExpressionContextParser)
  for tsSource <- TSSamples.samples do
    val tsDecl = TsDeclParser.parse(tsSource)
    println(tsDecl)

    println()
    println("### Run validator")
    val validationErrors = TyesValidator.validate(tsDecl.get)
    println(validationErrors.mkString("\r\n"))

    if validationErrors.isEmpty then  
      println()
      println(s"### Run interpreter")
      for e <- exps do
        println(s"${e} has type ${tyes.interpreter.TyesInterpreter.typecheck(tsDecl.get, e)}")
    
      println()
      println("### Run code generation")
      val src = TyesCodeGenerator.compile(tsDecl.get)
      println(src)
      println()

      val tsClassName = TyesCodeGenerator.getTypeSystemObjectName(tsDecl.get)
      val m = new javax.script.ScriptEngineManager(getClass().getClassLoader())
      val e = m.getEngineByName("scala")
      if e == null then
        println("No script engine found")
      else
        val rtTypeSystem = e.eval(src + s"\r\n${tsClassName}").asInstanceOf[tyes.runtime.TypeSystem[LExpression]]
        for e <- exps do
          println(s"${e} has type ${rtTypeSystem.typecheck(e)}")
