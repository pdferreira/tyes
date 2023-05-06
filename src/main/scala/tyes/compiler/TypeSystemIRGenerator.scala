package tyes.compiler

import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeTypeName
import tyes.model.*

val TCN = TargetCodeNode
val TCD = TargetCodeDecl
val TCTypeName = TargetCodeTypeName

class TypeSystemIRGenerator(commonEnvName: String):

  def generate(tsDecl: TypeSystemDecl): TargetCodeUnit =
    val className = s"${tsDecl.name.getOrElse("")}TypeSystem"
    TargetCodeUnit(className, Seq(
      TCD.Import(Seq("tyes", "runtime"), all = true),
      TCD.Import(Seq("example"), all = true),
      TCD.Class(
        className,
        inherits = Seq(
          TCTypeName("TypeSystem", TCTypeName("LExpression")),
          TCTypeName("TypeOperations")
        ),
        decls = Seq(
          TCD.Type("T", TCTypeName("Type"))
        )
      )
    ))
