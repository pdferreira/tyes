package tyes.compiler

import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeTypeName
import tyes.model.*
import utils.StringExtensions.*

val TCN = TargetCodeNode
val TCD = TargetCodeDecl
val TCTypeName = TargetCodeTypeName

class TypeSystemIRGenerator(commonEnvName: String):

  private val typeEnumTypeName = TCTypeName("Type")
  private val expClassTypeName = TCTypeName("LExpression")
  private val defaultEnvVarName = commonEnvName.decapitalize

  def generate(tsDecl: TypeSystemDecl): TargetCodeUnit =
    val className = s"${tsDecl.name.getOrElse("")}TypeSystem"
    val expVar: TCN.Var = TCN.Var("exp")

    TargetCodeUnit(className, Seq(
      TCD.Import(Seq("tyes", "runtime"), all = true),
      TCD.Import(Seq("example"), all = true),
      TCD.Class(
        className,
        inherits = Seq(
          TCTypeName("TypeSystem", expClassTypeName),
          TCTypeName("TypeOperations")
        ),
        decls = Seq(
          TCD.Type("T", typeEnumTypeName),
          TCD.Method(
            "typecheck",
            params = Seq(
              expVar.name -> expClassTypeName.copy(params = Seq(typeEnumTypeName)),
              defaultEnvVarName -> TCTypeName("Map", TCTypeName("String"), typeEnumTypeName)
            ),
            retTypeName = TCTypeName("Either", TCTypeName("String"), typeEnumTypeName),
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
