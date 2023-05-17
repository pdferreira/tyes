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
          typeIRGenerator.generateEnum(tsDecl),
          TCD.Method(
            "typecheck",
            params = Seq(
              expVar.name -> expClassTypeRef.copy(params = Seq(typeEnumTypeRef)),
              defaultEnvVarName -> TCTypeRef("Map", TCTypeRef("String"), typeEnumTypeRef)
            ),
            retTypeRef = TCTypeRef("Either", TCTypeRef("String"), typeEnumTypeRef),
            body = TCN.Match(
              expVar,
              branches = Seq(
                TCN.Var("_") -> TCN.Apply(
                  TCN.Var("Left"), 
                  TCN.FormattedText("TypeError: no type for `", expVar, "`")
                )
              )
            )
          )
        )
      )
    ))
