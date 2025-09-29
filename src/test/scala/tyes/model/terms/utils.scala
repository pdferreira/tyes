package tyes.model.terms

import scala.annotation.targetName

object TestTermBuilder extends TermBuilder[TestTerm, Any]:

  override def applyConstant(value: Any): TestTerm = TestTerm.Constant(value)

  override def unapplyConstant(term: TestTerm): Option[Any] = term match {
    case TestTerm.Constant(v) => Some(v)
    case _ => None
  }

  override def applyVariable(name: String): TestTerm & TermVariable = TestTerm.Variable(name)

  override def unapplyVariable(term: TestTerm): Option[String] = term match {
    case TestTerm.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: TestTerm*): TestTerm = TestTerm.Function(name, args*)

  override def unapplyFunction(term: TestTerm): Option[(String, Seq[TestTerm])] = term match {
    case TestTerm.Function(name, args*) => Some((name, args))
    case _ => None
  }

  override def applyRange(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[TestTerm],
    minIndex: Int,
    maxIndex: Index,
    holeIsMax: Boolean,
    holeSeed: Option[TestTerm] = None
  ): TestTerm & TermRange[TestTerm] = TestTerm.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed)

  override def unapplyRange(term: TestTerm): Option[(
    String,
    String,
    Int,
    Seq[TestTerm],
    Int,
    Index,
    Boolean,
    Option[TestTerm]
  )] = term match {
    case TestTerm.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      Some((function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed))
    case _ => None
  }

enum TestTerm extends TermOps[TestTerm, Any](TestTermBuilder):
  case Constant(value: Any)
  case Variable(name: String) extends TestTerm, TermVariable
  case Function(name: String, args: TestTerm*)
  case Range(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[TestTerm],
    minIndex: Int,
    maxIndex: Index,
    holeIsMax: Boolean,
    holeSeed: Option[TestTerm]
  ) extends TestTerm, TermRange[TestTerm]

def termFoldLeft1(function: String, seq: Seq[TestTerm]): TestTerm = {
  seq.tail.foldLeft(seq.head)(TestTerm.Function(function, _, _))
}

def termFoldLeft1(function: String, seed: TestTerm, holeIdx: Int, argsSeq: Seq[Seq[TestTerm]]): TestTerm = {
  argsSeq.foldLeft(seed) { (acc, args) =>
    TestTerm.Function(function, args.patch(from = holeIdx, Seq(acc), replaced = 0)*)
  }
}

def termFoldRight1(function: String, seq: Seq[TestTerm]): TestTerm = {
  seq.init.foldRight(seq.last)(TestTerm.Function(function, _, _))
}
  