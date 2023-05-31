package tyes.compiler

import java.nio.file.Path
import tyes.model.TypeSystemDecl
import tyes.compiler.ir.ScalaTargetCodeGenerator
import tyes.compiler.ir.TargetCodeIRGeneratorImpl

class TyesCompilerImpl extends TyesCompiler:

  override def compile(tsDecl: TypeSystemDecl): (Path, String) =
    val commonEnvName = TyesEnvDesugarer.inferEnvVarName(tsDecl).getOrElse("env")
    val desugaredTsDecl = new TyesEnvDesugarer(commonEnvName).desugar(tsDecl)

    val tcIRGenerator = new TargetCodeIRGeneratorImpl()
    val tsDeclCodeUnit = new TypeSystemIRGenerator(commonEnvName, tcIRGenerator).generate(desugaredTsDecl)
    val tcGenerator = new ScalaTargetCodeGenerator()
    (
      tcGenerator.getFileName(tsDeclCodeUnit),
      tcGenerator.generate(tsDeclCodeUnit)
    )
