package tyes.compiler

import tyes.model.*
import tyes.compiler.target.TargetCodeNode

class TargetCodeEnv(private val parent: Option[TargetCodeEnv] = None):

  private val identifiers = scala.collection.mutable.Set[String]()
  
  private val nameToCode = scala.collection.mutable.Map[String, TargetCodeNode]()
  
  private def nameclash(id: String): String =
    val id0 = parent.map(_.nameclash(id)).getOrElse(id)
    if identifiers.contains(id0) then
      val digitSuffix = id0.reverse.takeWhile(_.isDigit).reverse
      if digitSuffix.isEmpty then
        id0 + "2"
      else
        val baseName = id0.dropRight(digitSuffix.length)
        baseName + (digitSuffix.toInt + 1)
    else
      id0

  def registerIdentifier(name: String, idCode: TargetCodeNode): Boolean =
    if contains(name) then
      false
    else
      nameToCode += name -> idCode
      true

  def requestIdentifier(name: String): (String, TargetCodeNode) =
    val id = nameclash(name)
    val idCode = TargetCodeNode.Var(id)
    if registerIdentifier(name, idCode) then
      identifiers += name
      (id, idCode)
    else
      (name, this(name))

  def apply(name: String): TargetCodeNode =
    get(name).getOrElse(throw new NoSuchElementException(name))

  def get(name: String): Option[TargetCodeNode] =
    nameToCode.get(name).orElse(parent.flatMap(_.get(name)))

  def contains(name: String): Boolean = nameToCode.contains(name) || parent.map(_.contains(name)).getOrElse(false)

  def toMap: Map[String, TargetCodeNode] = 
    val parentMap = parent.map(_.toMap).getOrElse(Map())
    parentMap ++ nameToCode.toMap

  override def toString(): String = s"TargetCodeEnv(${toMap})"