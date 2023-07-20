package tyes.compiler

import java.nio.file.Path
import tyes.model.TypeSystemDecl
import tyes.compiler.ir.IRNodeSimplifier
import tyes.compiler.ir.TargetCodeIRGeneratorImpl
import tyes.compiler.target.ScalaTargetCodeAdapter
import tyes.compiler.target.ScalaTargetCodeGenerator
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeNodeSimplifier
import tyes.compiler.target.TargetCodeUnit

class TyesCompilerImpl extends TyesCompiler:

  private val TCN = TargetCodeNode

  override def compile(tsDecl: TypeSystemDecl): (Path, String) =
    val expVar: TCN.Var = TCN.Var("exp")
    val commonEnvName = TyesEnvDesugarer.inferEnvVarName(tsDecl).getOrElse("env")

    val tcIRGenerator = /*new IRNodeSimplifier().simplify
      .andThen(*/new TargetCodeIRGeneratorImpl(expVar).generate//)
    
    val tsDeclToScalaCode = 
      new TyesEnvDesugarer(commonEnvName).desugar
      .andThen(new TypeSystemIRGenerator(commonEnvName, expVar, tcIRGenerator).generate)
      .andThen(new TargetCodeNodeSimplifier().simplify)
      .andThen(new ScalaTargetCodeAdapter().adapt)
      .andThen(tcu => 
        val tcGenerator = new ScalaTargetCodeGenerator()
        (
          tcGenerator.getFileName(tcu),
          tcGenerator.generate(tcu)
        ))

    tsDeclToScalaCode.apply(tsDecl)
    
