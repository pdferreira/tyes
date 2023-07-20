package tyes.compiler.ir

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.model.*

enum IRNode:
  case Unexpected
  case Error(err: IRError)
  case Type(typ: IRType)
  case Switch(branches: Seq[(IRCond, IRNode)], otherwise: IRNode)
  case And(conds: Seq[IRCond], next: IRNode)
  case Or(main: IRNode, alternative: IRNode)

enum IRCond:
  case TypeDecl(
    declPat: TargetCodePattern,
    typExp: IRNode,
    expect: Option[IRTypeExpect] = None
    )
  case TypeEquals(t1Code: TargetCodeNode, t2Code: TargetCodeNode)
  case TermEquals(t1Code: TargetCodeNode, t2Code: TargetCodeNode)
  case OfType(termCode: TargetCodeNode, typRef: TargetCodeTypeRef)
  case EnvSizeIs(envVar: String, size: Int)
  case And(left: IRCond, right: IRCond)

enum IRType:
  case FromCode(typCode: TargetCodeNode, isOptional: Boolean = false)
  case Induction(expCode: TargetCodeNode, envCode: TargetCodeNode)
  case EnvGet(envVar: String, keyCode: TargetCodeNode)

enum IRTypeExpect:
  case EqualsTo(typCode: TargetCodeNode)
  case OfType(typRef: TargetCodeTypeRef)

enum IRError:
  case Generic(message: TargetCodeNode)
  case NoType(exp: TargetCodeNode)
  case UnexpectedType(obtained: TargetCodeNode, expected: TargetCodeNode)
