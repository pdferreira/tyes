package tyes.compiler

import java.nio.file.Path
import tyes.model.TypeSystemDecl

trait TyesCompiler:
  def compile(tsDecl: TypeSystemDecl): (Path, String)
