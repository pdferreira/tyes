package tyes.compiler.ir

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.model.*

enum IRNode:
  case Unexpected
  case Error(err: IRError)
  case Result(code: TargetCodeNode, canFail: Boolean)
  case Switch(branches: Seq[(TargetCodeNode, IRNode)], otherwise: IRNode)
  case And(conds: Seq[IRInstr], next: IRNode)
  case Or(main: IRNode, alternative: IRNode)

enum IRInstr:
  case Cond(cond: TargetCodeNode, err: IRError)
  case Check(exp: IRNode, resPat: TargetCodePattern)

enum IRError:
  case Generic(message: TargetCodeNode)
  case NoType(exp: TargetCodeNode)
  case UnexpectedType(obtained: TargetCodeNode, expected: TargetCodeNode)
