package tyes.runtime

import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import scala.reflect.ClassTag
import scala.runtime.BooleanRef

enum TestExp[+T]:
  case Const() extends TestExp[Nothing]
  case Binary(left: TestExp[T], right: TestExp[T])
  case Many(first: TestExp[T], second: Option[T], third: String, fourth: TestExp[T])

class TestTypeSystem extends TypeSystem[TestExp]:
    type T = TestType

    enum TestType extends Type:
      case One
    
    def typecheck(exp: TestExp[T], env: Environment[T]): Either[String, T] =
      Right(TestType.One)
    
    inline def callExtractRangeL[P <: TestExp[T]: ClassTag](exp: TestExp[T])(using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      }
    ) = exp.extractRangeL[P]
    
    inline def callExtractRangeR[P <: TestExp[T]: ClassTag](exp: TestExp[T])(using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      }
    ) = exp.extractRangeR[P]
    
    inline def callExtractRangeLNoSeed[P <: TestExp[T]: ClassTag](exp: TestExp[T])(using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      }
    ) = exp.extractRangeLNoSeed[P]
    
    inline def callExtractRangeRNoSeed[P <: TestExp[T]: ClassTag](exp: TestExp[T])(using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      }
    ) = exp.extractRangeRNoSeed[P]
  

class TypeSystemTests extends AnyFunSpec:
  import Assertions.*
  import TestExp.*

  val ts = new TestTypeSystem()
  
  describe("extractRangeL") {
    
    it("should return None for non-matching expression types") {
      val result = ts.callExtractRangeL[Binary[ts.T]](Const())
      assert(result.isEmpty)
    }
 
    it("should extract a tuple with the left-most expression and sequences of all the other arguments") {
      val innerLeft = Const()
      val innerRight = Many(Const(), Some(ts.TestType.One), "y", Const())
      val innerMany = Many(innerLeft, None, "x", innerRight): Many[ts.T]
      val outerMany = Many(innerMany, Some(ts.TestType.One), "z", Const()): Many[ts.T]
      
      val result: Option[(
        TestExp[ts.T],
        Seq[Option[ts.T]],
        Seq[String],
        Seq[TestExp[ts.T]]
      )] = ts.callExtractRangeL[Many[ts.T]](outerMany)

      assert(result.isDefined)
      assert(result.get == (
        innerLeft,
        Seq(innerMany.second, outerMany.second),
        Seq(innerMany.third, outerMany.third),
        Seq(innerMany.fourth, outerMany.fourth)
      ))
    }
    
  }

  describe("extractRangeR") {
    
    it("should return None for non-matching expression types") {
      val result = ts.callExtractRangeR[Many[ts.T]](Const())
      assert(result.isEmpty)
    }
 
    it("should extract a tuple with the right-most expression and sequences of all the other arguments") {
      val innerLeft = Many(Const(), Some(ts.TestType.One), "y", Const())
      val innerRight = Const()
      val innerMany = Many(innerLeft, None, "x", innerRight): Many[ts.T]
      val outerMany = Many(Const(), Some(ts.TestType.One), "z", innerMany): Many[ts.T]
      
      val result: Option[(
        Seq[TestExp[ts.T]],
        Seq[Option[ts.T]],
        Seq[String],
        TestExp[ts.T]
      )] = ts.callExtractRangeR[Many[ts.T]](outerMany)

      assert(result.isDefined)
      assert(result.get == (
        Seq(outerMany.first, innerMany.first),
        Seq(outerMany.second, innerMany.second),
        Seq(outerMany.third, innerMany.third),
        innerRight
      ))
    }
    
  }

  describe("extractRangeLNoSeed") {
    
    it("should return None for non-matching expression types") {
      val result = ts.callExtractRangeLNoSeed[Binary[ts.T]](Many(Const(), None, "a", Const()))
      assert(result.isEmpty)
    }
 
    it("should extract a sequence with the left-most expression as seed and all the right arguments") {
      val innerLeft = Const()
      val innerRight = Binary(Const(), Const())
      val innerBinary = Binary(innerLeft, innerRight): Binary[ts.T]
      val outerBinary = Binary(innerBinary, Const()): Binary[ts.T]
      
      val result: Option[Seq[TestExp[ts.T]]] = ts.callExtractRangeLNoSeed[Binary[ts.T]](outerBinary)

      assert(result.isDefined)
      assert(result.get == Seq(innerLeft, innerRight, outerBinary.right))
    }
    
  }

  describe("extractRangeRNoSeed") {
    
    it("should return None for non-matching expression types") {
      val result = ts.callExtractRangeRNoSeed[Binary[ts.T]](Const())
      assert(result.isEmpty)
    }
 
    it("should extract a sequence with the right-most expression as seed and all the left arguments") {
      val innerLeft = Binary(Const(), Const())
      val innerRight = Const()
      val innerBinary = Binary(innerLeft, innerRight): Binary[ts.T]
      val outerBinary = Binary(Const(), innerBinary): Binary[ts.T]
      
      val result: Option[Seq[TestExp[ts.T]]] = ts.callExtractRangeRNoSeed[Binary[ts.T]](outerBinary)

      assert(result.isDefined)
      assert(result.get == Seq(outerBinary.left, innerLeft, innerRight))
    }
    
  }

  