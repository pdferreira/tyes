package utils.parsers

import scala.util.parsing.input.*

class SeqPosition[T](seq: Seq[T], override val column: Int) extends Position:

  override def line: Int = 1

  override def lineContents: String = throw new NotImplementedError()

  override def longString: String =
    val sep = " "
    val elemsWithPositions = seq.map(_.toString).foldLeft(Seq[(String, Int)]()) { 
      case (acc, text) => acc match {
          case Seq() => Seq(text -> 0)
          case _ :+ (prevText, prevPos) => acc :+ (text, prevPos + prevText.length + sep.length)
      }
    }
    val line = elemsWithPositions.map(_._1).mkString(" ")
    val arrowPos = 
      if elemsWithPositions.isEmpty 
      then 0 
      else elemsWithPositions(column - 1)._2

    line + "\r\n" + " ".repeat(arrowPos) + "^"
