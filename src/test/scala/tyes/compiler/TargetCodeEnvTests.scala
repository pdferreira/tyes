package tyes.compiler

import org.scalatest.*
import funspec.*
import tyes.model.indexes.*
import tyes.model.Term
import tyes.compiler.target.TargetCodeNode

class TargetCodeEnvTests extends AnyFunSpec:
  import Assertions.*
  
  val varA = Term.Variable("a")
  val varB = Term.Variable("b")
  val externalVar = Term.Variable("external")

  describe("An environment") {

    {
      val env = new TargetCodeEnv()
      val (aId, aCode) = env.requestIdentifier(varA)

      describe("can retrieve code") {
        
        describe("mapped by term variable") {

          it("if it has a mapping") {
            assert(env(varA) == aCode)
          }

          it("even if it has multiple mappings (returns the first)") {
            env.requestIdentifier(varA)
            assert(env(varA) == aCode)
          }

          describe("if it is composed by a variable that has a plural mapping") {

            val varBs = Term.Variable(varB.name + "s")
            val (_, bsCode) = env.requestIdentifier(varBs)

            it("and a numeric index") {
              val idx = 5
              val varB5 = Term.Variable(indexedVar(varB.name, idx.toString))
              assert(env(varB5) == TCN.Index(bsCode, TCN.Integer(5)))
            }

            it("and a variable index that has a mapping") {
              val varBA = Term.Variable(indexedVar(varB.name, varA.name))
              assert(env(varBA) == TCN.Index(bsCode, aCode))
            }
          }

        }

        describe("mapped by id") {

          it("if it has a mapping") {
            assert(env(aId) == aCode)
          }

          it("even if the id resulted from nameclashing") {
            val (otherAId, otherACode) = env.requestIdentifier(varA)
            assert(env(otherAId) == otherACode)
          }
        }

      }
      
      describe("fails to retrieve code") {

        it("mapped to non registered term variable") {
          intercept [NoSuchElementException] {
            env(varB)
          }
        }

        it("mapped to non registered id") {
          val otherEnv = new TargetCodeEnv()
          val (externalId, _) = otherEnv.requestIdentifier(externalVar)
          intercept [NoSuchElementException] {
            env(externalId)
          }
        }

      }

      describe("can register new code mappings to a term variable") {
        
        it("defaulting to a variable code with the same name") {
          val (_, bCode) = env.requestIdentifier(varB)
          assert(bCode == TargetCodeNode.Var(varB.name))
        }

        it("nameclashing the variable code name if already existed") {
          val (_, otherACode) = env.requestIdentifier(varA)
          assert(otherACode != env(aId))
        }

      }

      describe("can retrieve all indexes with a common root variable") {

        it("returning none if the variable is never indexed") {
          assert(env.getIndexes(varA.name).isEmpty)
        }

        it("returning all integer indexes if the variable is indexed") {
          val rootVarName = "t"
          
          for idxStr <- Seq("1", "3", "i") do
            env.requestIdentifier(Term.Variable(indexedVar(rootVarName, idxStr)))

          assert(env.getIndexes(rootVarName) == Set(1, 3)) 
        }

      }

    }

    describe("with a parent") {

      val parentEnv = new TargetCodeEnv()
      val (parentAId, parentACode) = parentEnv.requestIdentifier(varA)

      val env = new TargetCodeEnv(parentEnv)
      val (localAId, localACode) = env.requestIdentifier(varA)  

      it("gives preference to locally registered mappings") {
        assert(env(varA) == localACode)
      }

      it("has access to parent id mappings") {
        assert(env(parentAId) == parentACode)
      }

      it("nameclashes against parent code") {
        assert(env(varA) != parentEnv(varA))
      }

      it("doesn't register new ids in parent") {
        intercept [NoSuchElementException] {
          parentEnv(localAId)
        }
      }

      it("includes parent variable indexes") {
        val rootVarName = "t"
        parentEnv.requestIdentifier(Term.Variable(indexedVar(rootVarName, "2")))
        env.requestIdentifier(Term.Variable(indexedVar(rootVarName, "5")))
        assert(env.getIndexes(rootVarName) == Set(2, 5))
      }

    }

  }
