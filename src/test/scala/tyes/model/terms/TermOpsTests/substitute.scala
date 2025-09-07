package tyes.model.terms.TermOpsTests

import org.scalatest.*
import funspec.*
import tyes.model.indexes.*
import tyes.model.terms.*

class substitute extends AnyFunSpec:
  import Assertions.*
  import TestTerm.*

  describe("A Variable term") {

    val varTerm = Variable("x")
    val constTerm = Constant(5)

    it("is replaced by substitution where its name is bound") {
      assert(varTerm.substitute(Map("x" -> constTerm)) eq constTerm)
    }

    it("is not replaced by substitution where its name is not bound") {
      assert(varTerm.substitute(Map("y" -> constTerm)) eq varTerm)
    }

  }

  describe("A Function term") {

    val funTerm = Function("f", Seq("x", "y", "x").map(Variable.apply)*)

    it("recursively propagates substitutions to its arguments") {
      val subst = Map("x" -> Constant(3), "y" -> Constant("a"), "f" -> Constant("g"))
      val expectedArgs = Seq(Constant(3), Constant("a"), Constant(3))
      assert(funTerm.substitute(subst) == Function("f", expectedArgs*))
    }

  }

  describe("A Range term") {

    val function = "f"
    val cursor = "i"
    val rootVar = "t"
    val template = Function("C", Variable(indexedVar(rootVar, cursor)))
    val values = Seq(Constant(1), Constant("a"), Constant(true))

    describe("with limited bounds") {

      it("is replaced by a left-associative function term") {
        val range = Range(function, cursor, template, 0, Index.Number(2), None)
        val subst = Map(
          indexedVar(rootVar, "1") -> Constant("a"),
          indexedVar(rootVar, "2") -> Constant(5)
        )
        val expectedArgValues = Seq(
          Variable(indexedVar(rootVar, "0")),
          Constant("a"),
          Constant(5)
        )
        val expectedFunTerm = termFoldLeft1(function, expectedArgValues.map(Function("C", _)))
        assert(range.substitute(subst) == expectedFunTerm)
      }

      it("replaces substitution-matching free variables in the seed") {
        val seed = Function("s", Variable("w"))
        val range = Range(function, cursor, template, 0, Index.Number(0), Some(seed))
        val subst = Map(
          indexedVar(rootVar, "0") -> Constant("zero"),
          "w" -> Constant(true)
        )
        val expectedFunTerm = Function(function,
          Function("s", Constant(true)),
          Function("C", Constant("zero"))
        )
        assert(range.substitute(subst) == expectedFunTerm)
      }
    }

    describe("with unlimited bounds") {

      val boundsVar = "k"

      describe("if the bound is closed by substitution") {
        
        it("is replaced by a left-associative function term") {
          val range = Range(function, cursor, template, 0, Index.Variable(boundsVar), None)
          val subst = Map(
            boundsVar -> Constant(3),
            indexedVar(rootVar, "1") -> Constant("a"),
            indexedVar(rootVar, "3") -> Constant(5)
          )
          val expectedArgValues = Seq(
            Variable(indexedVar(rootVar, "0")),
            Constant("a"),
            Variable(indexedVar(rootVar, "2")),
            Constant(5)
          )
          val expectedFunTerm = termFoldLeft1(function, expectedArgValues.map(Function("C", _)))
          assert(range.substitute(subst) == expectedFunTerm)
        }

        it("replaces substitution-matching free variables in the seed") {
          val seed = Function("s", Variable("w"))
          val range = Range(function, cursor, template, 0, Index.Variable(boundsVar), Some(seed))
          val subst = Map(
            boundsVar -> Constant(0),
            indexedVar(rootVar, "0") -> Constant("zero"),
            "w" -> Constant(true)
          )
          val expectedFunTerm = Function(function,
            Function("s", Constant(true)),
            Function("C", Constant("zero"))
          )
          assert(range.substitute(subst) == expectedFunTerm)
        }
      }

      describe("if the bound is not closed by substitution") {

        it("propagates substitution to its maxIndex, template and seed") {
          val seed = Function("s", Variable("w"))
          val template = Function("t", Variable(indexedVar(rootVar, cursor)), Variable("w"))
          val minIndex = 0
          val maxIndex = Index.Variable(boundsVar, 5)
          val range = Range(function, cursor, template, minIndex, maxIndex, Some(seed))

          val subst = Map(
            "w" -> Constant(true),
            boundsVar -> Variable("b")
          )

          val expectedRange = Range(
            function,
            cursor,
            Function("t", Variable(indexedVar(rootVar, cursor)), Constant(true)),
            minIndex,
            Index.Variable("b", min = 5),
            Some(Function("s", Constant(true)))
          )
          assert(range.substitute(subst) == expectedRange)
        }

      }
    }
  }

  