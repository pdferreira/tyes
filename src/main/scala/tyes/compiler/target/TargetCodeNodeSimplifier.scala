package tyes.compiler.target

import TargetCodeNodeOperations.*

class TargetCodeNodeSimplifier:
  
  def simplify(tcUnit: TargetCodeUnit): TargetCodeUnit =
    tcUnit.copy(decls = tcUnit.decls.map(simplify))

  def simplify(tcDecl: TargetCodeDecl): TargetCodeDecl = applyToChildren(tcDecl, n => simplify(n))

  def simplify(tcNode: TargetCodeNode): TargetCodeNode = applyUntil(tcNode, {
    case n1 @ TCN.Let(TCP.Var(name1), exp1, n2 @ TCN.Let(TCP.Var(name2), exp2, body)) if name1 != name2 =>
      val exp2FNs = freeNames(exp2).count(name1)
      val bodyFNs = freeNames(body).count(name1)
      if exp2FNs + bodyFNs == 1 then
        if exp2FNs == 1 then
          simplify(TCN.Let(TCP.Var(name2), replace(exp2, name1, exp1), body))
        else
          simplify(TCN.Let(TCP.Var(name2), exp2, replace(body, name1, exp1)))
      else
        n1

    case TCN.For(TCFC.Let(pat, exp) :: cs, bodyExp) =>
      simplify(TCN.Let(pat, exp, TCN.For(cs, bodyExp)))

    case TCN.For(TCFC.Iterate(TCP.Var(name1), colExp) :: Nil, TCN.Var(name2)) if name1 == name2 =>
      simplify(colExp)
  })
