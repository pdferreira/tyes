package utils.parsers

import scala.util.parsing.input.*

class SeqReader[T](seq: Seq[T], offset: Int = 0) extends Reader[T]:
  
  override def first: T = seq(offset)
  
  override def rest: Reader[T] =
    if atEnd 
    then this
    else new SeqReader(seq, offset + 1)
  
  override def atEnd: Boolean = offset >= seq.length
  
  override def pos: Position = new SeqPosition(seq, offset + 1)
