package tyes.model

import scala.collection.mutable.ListBuffer
import tyes.model.indexes.*
import tyes.model.terms.*

object ranges:
  
  def extractRangeVariable(from: Judgement, to: Judgement): (String, Range) =
    val HasType(Term.Variable(fromVar), _) = from.assertion: @unchecked
    val HasType(Term.Variable(toVar), _) = to.assertion: @unchecked

    val Some((fromIdent, fromIdx)) = extractIntIndex(fromVar): @unchecked
    val Some((toIdent, toIdxStr)) = extractIndex(toVar): @unchecked

    val toIdx = toIdxStr.toIntOption.getOrElse(Int.MaxValue)
    (fromIdent, Range.inclusive(fromIdx, toIdx))

  def extractTermRange(funName: String, start: Term, end: Term, seed: Option[Term], minOccurs: Int): Either[List[String], Term.Range] =
    extractTermRange(funName, start, end, seed, minOccurs, Term.Range.apply)

  def extractTypeRange(funName: String, start: Type, end: Type, seed: Option[Type], minOccurs: Int): Either[List[String], Type.Range] =
    extractTermRange(funName, start, end, seed, minOccurs, Type.Range.apply)

  def extractTermRange[TTerm <: terms.TermOps[TTerm, TConstant], TConstant, TRange](
    funName: String,
    start: TTerm,
    end: TTerm,
    seed: Option[TTerm],
    minOccurs: Int,
    createRange: (String, String, TTerm, Int, Index, Option[TTerm]) => TRange
  ): Either[List[String], TRange] =
    val startIndexedVars = start.variables.collect(extractIndex.unlift)
    val endIndexedVars = end.variables.collect(extractIndex.unlift)
    val startNonSharedIndexedVars = startIndexedVars -- endIndexedVars
    val endNonSharedIndexedVars = endIndexedVars -- startIndexedVars
    val startIndexes = startNonSharedIndexedVars.map(_._2).toSet.map(_.toIntOption)
    var errors = ListBuffer.empty[String]
    if startIndexes.size != 1 then
      errors += "start should have a common index throughout at least one indexed variable"
    else if startIndexes.head.isEmpty then
      errors += "start should have an integer common index"
    
    val endIndexes = endNonSharedIndexedVars.map(_._2).toSet
    if endIndexes.size != 1 then
      errors += "end should have a common index"
    else if errors.isEmpty then
      val startIndex = startIndexes.head.get
      val endIndex = endIndexes.head
      val cursor = "$i"
      val template = start.replaceIndex(startIndex.toString, cursor)
      if template != end.replaceIndex(endIndex.toString, cursor) then
        errors += "start and end should be the same modulo their respective main index"
      if startIndex >= endIndex.toIntOption.getOrElse(Int.MaxValue) then
        errors += "start index should be less than the end index"
      
      if errors.isEmpty then
        return Right(createRange(
          funName,
          cursor,
          template,
          startIndex,
          endIndex.toIntOption match {
            case None => Index.Variable(endIndex, minOccurs)
            case Some(i) => Index.Number(i)
          },
          seed,
        ))
    
    return Left(errors.toList)

  def getRangeElems[TTerm <: terms.TermOps[TTerm, TConstant], TConstant, TElem](
    range: terms.TermRange[TTerm],
    getElems: TTerm => Iterable[TElem]
  ): Iterable[TElem] =
    val indexes = range.maxIndex match {
      case Index.Number(i) => Set(range.minIndex, i).map(_.toString)
      case Index.Variable(s, _) => Set(range.minIndex.toString, s)
    }
    val seedElems = range.seed.map(getElems(_)).getOrElse(Iterable())
    val expandedElems = indexes.flatMap(i => getElems(range.template.replaceIndex(range.cursor, i)))
    expandedElems ++ seedElems

