package tyes.compiler

import tyes.model.*

object TyesValidator:
  def validate(tsDecl: TypeSystemDecl): Seq[String] =
    (for
      case Seq(r1, r2) <- tsDecl.rules.combinations(2)
      HasType(e1, t1) = r1.conclusion
      HasType(e2, t2) = r2.conclusion
      if t1 != t2 && e1.overlaps(e2)
    yield
      val r1Name = r1.name.getOrElse(s"Rule #${tsDecl.rules.indexOf(r1)}")
      val r2Name = r2.name.getOrElse(s"Rule #${tsDecl.rules.indexOf(r2)}")
      s"Error: conclusions of ${r1Name} and ${r2Name} overlap but result in different types"
    ).toSeq
