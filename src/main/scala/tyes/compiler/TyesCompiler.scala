package tyes.compiler

import tyes.model.TypeSystemDecl
import tyes.runtime.TypeSystem

trait TyesCompiler:
  def compile(tsDecl: TypeSystemDecl): String
  def getFileName(tsDecl: TypeSystemDecl): String
