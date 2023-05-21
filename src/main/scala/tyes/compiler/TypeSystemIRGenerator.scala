package tyes.compiler

import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.model.*
import utils.StringExtensions.*

private val TCN = TargetCodeNode
private val TCD = TargetCodeDecl
private val TCTypeRef = TargetCodeTypeRef

class TypeSystemIRGenerator(commonEnvName: String):

  private val expClassTypeRef = TCTypeRef("LExpression")
  private val defaultEnvVarName = commonEnvName.decapitalize

  private val typeIRGenerator = new TypeIRGenerator()
  private val termIRGenerator = new TermIRGenerator(typeIRGenerator)

  def generate(tsDecl: TypeSystemDecl): TargetCodeUnit =
    val className = s"${tsDecl.name.getOrElse("")}TypeSystem"
    val expVar: TCN.Var = TCN.Var("exp")
    val typeEnumTypeRef = typeIRGenerator.typeEnumTypeRef

    TargetCodeUnit(className, Seq(
      TCD.Import(Seq("tyes", "runtime"), all = true),
      TCD.Import(Seq("example"), all = true),
      TCD.Class(
        className,
        inherits = Seq(
          TCTypeRef("TypeSystem", expClassTypeRef),
          TCTypeRef("TypeOperations")
        ),
        decls = Seq(
          TCD.Type("T", typeEnumTypeRef),
          typeIRGenerator.generateDecl(tsDecl),
          TCD.Method(
            "typecheck",
            params = Seq(
              expVar.name -> expClassTypeRef.copy(params = Seq(typeEnumTypeRef)),
              defaultEnvVarName -> TCTypeRef("Map", TCTypeRef("String"), typeEnumTypeRef)
            ),
            retTypeRef = TCTypeRef("Either", TCTypeRef("String"), typeEnumTypeRef),
            body = generateTypecheckBody(expVar, tsDecl.rules)
          )
        )
      )
    ))

  private def generateTypecheckBody(expVar: TCN.Var, rules: Seq[RuleDecl]): TargetCodeNode =
    val defaultCase = TCN.Var("_") -> TCN.Apply(
      TCN.Var("Left"), 
      TCN.FormattedText("TypeError: no type for `", expVar, "`")
    )

    val ruleCases = rules
      .map(r => r.conclusion.assertion match {
        case HasType(term, _) => extractTemplate(term)
      })
      .distinct
      .map(rt => termIRGenerator.generate(rt) -> TCN.Var("???"))

    TCN.Match(
      expVar,
      branches = ruleCases :+ defaultCase
    )

  private def extractTemplate(term: Term): Term = term match {
    case Term.Function(fnName, args*) =>
      def getSuffix(idx: Int): String =
        if args.length == 1 
        then "" 
        else (idx + 1).toString
  
      val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
        arg match {
          case Term.Variable(_) => arg
          case Term.Constant(_) =>
            // Simple naming heuristic based on the constructor name, while field
            // names are not considered.
            val initial = fnName.findLast(_.isUpper).map(_.toLower).getOrElse('c')
            Term.Variable(initial + getSuffix(idx))
          case Term.Function(_, _*) => Term.Variable("e" + getSuffix(idx))
          case Term.Type(typ) => Term.Type(typ match {
            case Type.Variable(_) => typ
            case Type.Named(_) => Type.Variable("ct" + getSuffix(idx))
            case Type.Composite(_, _*) => Type.Variable("ct" + getSuffix(idx))
          })
        }
      }
      Term.Function(fnName, argsAsVariables*)
    case _ => term
  }

