package tyes.compiler

import tyes.model.*
import tyes.compiler.target.TargetCodeNode

object TargetCodeEnv:

  opaque type Id = String

  extension (name: String)

    protected inline def toId: Id = name

  extension (id: Id)

    protected inline def asString: String = id

  protected def splitDigitSuffix(id: Id): (String, Option[Int]) = 
    val suffix = id.reverse.takeWhile(_.isDigit).reverse
    val prefix = id.dropRight(suffix.length)
    (prefix, if suffix.isEmpty then None else Some(suffix.toInt))


class TargetCodeEnv(private val parent: Option[TargetCodeEnv] = None):
  import TargetCodeEnv.*
  private val TCN = TargetCodeNode

  private type TermVar = Term.Variable | Type.Variable

  private val idToCode = scala.collection.mutable.Map[Id, TargetCodeNode]()
  private val nameToIds = scala.collection.mutable.Map[String, Seq[Id]]()
  
  private def nameclash(name: String): Id =
    val id0 = parent.map(_.nameclash(name)).getOrElse(name.toId)
    if idToCode.contains(id0) then
      val (prefix, digitSuffix) = splitDigitSuffix(id0)
      digitSuffix match {
        case None => nameclash(prefix + "2")
        case Some(n) => nameclash(prefix + (n + 1))
      }
    else
      id0

  def requestIdentifier(termVar: TermVar): (Id, TargetCodeNode.Var) =
    val id = nameclash(termVar.name)
    val idCode: TCN.Var = TCN.Var(id.toString)
    
    idToCode(id) = idCode
    nameToIds(termVar.name) = nameToIds.get(termVar.name) match {
      case None => Seq(id)
      case Some(ids) => ids :+ id
    }

    (id, idCode)

  def apply(id: Id): TargetCodeNode =
    get(id).getOrElse(throw new NoSuchElementException(id.asString))

  def apply(termVar: TermVar): TargetCodeNode =
    val ids = getIds(termVar).getOrElse(throw new NoSuchElementException(termVar.toString))
    apply(ids.head)

  private def get(id: Id): Option[TargetCodeNode] =
    idToCode.get(id).orElse(parent.flatMap(_.get(id)))

  private def getIds(termVar: TermVar): Option[Seq[Id]] =
    nameToIds.get(termVar.name).orElse(parent.flatMap(_.getIds(termVar)))

  def contains(termVar: TermVar): Boolean = getIds(termVar).isDefined

  override def toString(): String =
    val parentStr = parent.map(" <- " + _.toString).getOrElse("")
    val idToCodeStr = idToCode.mkString(", ")
    val nameToIdsStr = nameToIds.mkString(", ")
    s"TargetCodeEnv(${idToCodeStr})[${nameToIdsStr}]$parentStr"
