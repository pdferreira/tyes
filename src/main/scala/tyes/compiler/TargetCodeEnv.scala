package tyes.compiler

import tyes.model.*
import tyes.model.terms.TermVariable
import tyes.compiler.target.TargetCodeNode

object TargetCodeEnv:

  opaque type Id = String

  extension (name: String)

    protected inline def toId: Id = name

  extension (id: Id)

    protected inline def asString: String = id


class TargetCodeEnv(private val parent: Option[TargetCodeEnv] = None):
  import TargetCodeEnv.*
  private val TCN = TargetCodeNode

  private val idToCode = scala.collection.mutable.Map[Id, TargetCodeNode]()
  private val nameToIds = scala.collection.mutable.Map[String, Seq[Id]]()
  
  def this(parent: TargetCodeEnv) = this(Some(parent))

  def this() = this(None)

  private def allIds: Set[Id] = 
    val localIds: Set[Id] = idToCode.keys.toSet
    val parentIds: Set[Id] = parent.map(_.allIds).getOrElse(Set[Id]())
    localIds ++ parentIds

  private def nameclash(name: String): Id =
    NameOperations.nameclash(name, allIds.map(_.asString)).toId

  def requestIdentifier(termVar: TermVariable): (Id, TargetCodeNode.Var) =
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

  def apply(termVar: TermVariable): TargetCodeNode =
    val ids = getIds(termVar).getOrElse(throw new NoSuchElementException(termVar.toString))
    apply(ids.head)

  private def get(id: Id): Option[TargetCodeNode] =
    idToCode.get(id).orElse(parent.flatMap(_.get(id)))

  private def getIds(termVar: TermVariable): Option[Seq[Id]] =
    nameToIds.get(termVar.name).orElse(parent.flatMap(_.getIds(termVar)))

  def contains(termVar: TermVariable): Boolean = getIds(termVar).isDefined

  def getIndexes(indexedName: String): Set[Int] =
    nameToIds.keys
      .map(_.split("_", 2))
      .collect({ case Array(name, idxStr) if name == indexedName && idxStr.matches(raw"\d+") => idxStr.toInt })
      .toSet
      .union(parent.map(_.getIndexes(indexedName)).getOrElse(Set()))

  override def toString(): String =
    val parentStr = parent.map(" <- " + _.toString).getOrElse("")
    val idToCodeStr = idToCode.mkString(", ")
    val nameToIdsStr = nameToIds.mkString(", ")
    s"TargetCodeEnv(${idToCodeStr})[${nameToIdsStr}]$parentStr"
