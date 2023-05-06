package tyes.compiler

import java.nio.file.Path
import tyes.model.TypeSystemDecl
import tyes.runtime.TypeSystem

trait TyesCompiler:
  def compile(tsDecl: TypeSystemDecl): (Path, String)
