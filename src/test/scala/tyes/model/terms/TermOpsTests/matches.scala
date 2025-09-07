package tyes.model.terms.TermOpsTests

import org.scalatest.*
import funspec.*
import tyes.model.indexes.*
import tyes.model.terms.*

class matches extends AnyFunSpec:
  import Assertions.*
  import TestTerm.*

  describe("A Variable term") {

    for term <- Seq(
      Constant(3),
      Variable("z"),
      Function("f", Constant(true), Variable("a")),
      Range("p", "i", Variable("e_i"), 0, Index.Number(5), Some(Constant(7)))
    ) do
      it(s"matches any ${term.getClass().getSimpleName()} term") {
        assert(Variable("x").matches(term) == Some(Map("x" -> term)))
      }

    it("matches an equal variable with an empty substitution") {
      assert(Variable("x").matches(Variable("x")) == Some(Map()))
    }

  }

  describe("A Range term") {

    val function = "f"
    val cursor = "i"
    val rootVar = "t"
    val template = Function("C", Variable(indexedVar(rootVar, cursor)))
    val values = Seq(Constant(1), Constant("a"), Constant(true))

    describe("with unlimited bounds") {

      val boundsVar = "k"

      describe("and no seed") {
        val range = Range(function, cursor, template, 0, Index.Variable(boundsVar), None)

        it("matches a left-associative term with the target function") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1(function, args)
          
          val argsSubst = Map.from(
            for case (t, i) <- values.zipWithIndex
            yield indexedVar(rootVar, i.toString) -> t
          )
          assert(range.matches(term) == Some(argsSubst + (boundsVar -> Constant(values.length - 1))))
        }

        it("matches a single term matching the template") {
          val term = Function("C", values(0))
          assert(range.matches(term) == Some(Map("t_0" -> values(0), boundsVar -> Constant(0))))
        }

        it("matches a range with matching template modulo their cursors") {
          val r1 = Range("f", "i", Function("P", Variable("e_i"), Variable("y")), 0, Index.Variable("k"), None)
          val r2 = Range("f", "j", Function("P", Variable("e_j"), Constant(1)), 0, Index.Number(5), None)
          assert(r1.matches(r2) == Some(Map("y" -> Constant(1), "k" -> Constant(5))))
        }

        it("does not match a left-associative term with sub-terms not matching the template") {
          val args = values
          val term = termFoldLeft1(function, args)
          assert(range.matches(term) == None)
        }

        it("does not match a left-associative term with a different function") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1("g", args)
          assert(range.matches(term) == None)
        }
        
        it("does not match a left-associative term with a constant") {
          val term = Constant(1)
          assert(range.matches(term) == None)
        }

        it("does not match a range in a way that substitutes its ranged-over variables") {
          val otherRange = Range("f", "j", Function("P", Variable("e_j"), Constant(1)), 0, Index.Number(5), None)
          assert(range.matches(otherRange) == None)
        }

        it("does not match a range with a seed") {
          val otherRange = Range(function, cursor, template, range.minIndex, range.maxIndex, Some(Constant(2)))
          assert(range.matches(otherRange) == None)
        }
      }

      describe("and a seed") {
        val seed = Function("K", Variable("a"))
        val range = Range("f", "i", Function("C", Variable("t_i")), 1, Index.Variable("k"), Some(seed))
        val concreteSeed = Function("K", Constant(true))
        
        it("matches a term that matches the seed") {
          val term = concreteSeed
          assert(range.matches(term) == Some(Map("a" -> Constant(true), "k" -> Constant(0))))
        }

        it("matches a term that matches the seed at the innermost position") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1(function, concreteSeed +: args)
          val argsSubst = Map.from(
            for case (t, i) <- values.zipWithIndex
            yield f"t_${i+1}" -> t
          )
          assert(range.matches(term) == Some(
            argsSubst + 
            ("k" -> Constant(values.length)) + 
            ("a" -> Constant(true))
          ))
        }

        it("matches a range with matching template modulo their cursors") {
          val r1 = Range("f", "i", Function("P", Variable("e_i"), Variable("y")), 0, Index.Variable("k"), Some(Variable("y")))
          val r2 = Range("f", "j", Function("P", Variable("e_j"), Constant(1)), 0, Index.Number(5), Some(Constant(1)))
          assert(r1.matches(r2) == Some(Map("y" -> Constant(1), "k" -> Constant(5))))
        }

        it("does not match a range with a non-matching seed") {
          val otherRange = Range(function, cursor, template, range.minIndex, range.maxIndex, Some(Constant(2)))
          assert(range.matches(otherRange) == None)
        }
      }

      describe("and a minimum of occurrences") {
        val range = Range("f", "i", Function("C", Variable("t_i")), 0, Index.Variable("k", min = 2), None)

        it("matches a term with greater occurrences") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1(function, args)
          val argsSubst = Map.from(
            for case (t, i) <- values.zipWithIndex
            yield f"t_$i" -> t
          )
          assert(range.matches(term) == Some(argsSubst + ("k" -> Constant(values.length - 1))))
        }

        it("doesn't match a term with lower occurrences") {
          val term = Function("C", values(0))
          assert(range.matches(term) == None)
        }

      }
    }

    describe("with limited bounds") {
      
      describe("and no seed") {
        val range = Range(function, cursor, template, 0, Index.Number(2), None)

        it("matches a left-associative term with the target function") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1(function, args)
          
          val argsSubst = Map.from(
            for case (t, i) <- values.zipWithIndex
            yield f"t_$i" -> t
          )
          assert(range.matches(term) == Some(argsSubst))
        }

        it("does not match a term with sub-terms below the bound") {
          val args = values.take(2).map(Function("C", _))
          val term = termFoldLeft1(function, args)
          assert(range.matches(term) == None)
        }

        it("does not match a term with sub-terms above the bound") {
          val args = (values ++ values).map(Function("C", _))
          val term = termFoldLeft1(function, args)
          
          assert(range.matches(term) == None)
        }
      }

      describe("and a seed") {
        val seed = Function("K", Variable("a"))
        val range = Range(function, cursor, template, 1, Index.Number(3), Some(seed))
        val concreteSeed = Function("K", Constant(true))

        it("does not match a term that only matches the seed") {
          val term = concreteSeed
          assert(range.matches(term) == None)
        }

        it("matches a term that matches the seed at the innermost position") {
          val args = values.map(Function("C", _))
          val term = termFoldLeft1(function, concreteSeed +: args)
          val argsSubst = Map.from(
            for case (t, i) <- values.zipWithIndex
            yield f"t_${i+1}" -> t
          )
          assert(range.matches(term) == Some(
            argsSubst +  
            ("a" -> Constant(true))
          ))
        }
      }
    }
  }

  describe("A Function term") {

    it("matches a bounded Range term with matching function") {
      val argVars = Seq("a", "b", "c")
      val funTerm = termFoldLeft1("f", argVars.map(Variable(_)))
      val rangeTerm = Range("f", "i", Variable("e_i"), 5, Index.Number(7), None)

      val argsSubst = Map.from(
        for case (v, i) <- argVars.zipWithIndex
        yield v -> Variable(f"e_${i+5}")
      )
      assert(funTerm.matches(rangeTerm) == Some(argsSubst))
    }

    it("doesn't match a bounded Range term with incompatible bounds") {
      val argVars = Seq("a", "b", "c")
      val funTerm = termFoldLeft1("f", argVars.map(v => Function("C", Variable(v))))
      val rangeTerm = Range("f", "i", Function("C", Variable("e_i")), 5, Index.Number(9), None)

      assert(funTerm.matches(rangeTerm) == None)
    }

    it("doesn't match a bounded Range term with a different function") {
      val argVars = Seq("a", "b", "c")
      val funTerm = termFoldLeft1("f", argVars.map(Variable(_)))
      val rangeTerm = Range("g", "i", Variable("e_i"), 5, Index.Number(7), None)

      assert(funTerm.matches(rangeTerm) == None)
    }

  }

  