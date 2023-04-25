package tyes.compiler

import tyes.model.*
import tyes.compiler.ir.CodeGenNode

class CodeEnv(private val parent: Option[CodeEnv] = None):

  private val identifiers = scala.collection.mutable.Set[String]()
  
  private val nameToCode = scala.collection.mutable.Map[String, CodeGenNode]()
  
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

  def registerIdentifier(name: String, idCode: CodeGenNode): Boolean =
    if contains(name) then
      false
    else
      nameToCode += name -> idCode
      true

  def requestIdentifier(name: String): (String, CodeGenNode) =
    val id = nameclash(name)
    val idCode = CodeGenNode.Var(id)
    if registerIdentifier(name, idCode) then
      identifiers += name
      (id, idCode)
    else
      (name, this(name))

  def apply(name: String): CodeGenNode =
    nameToCode.get(name).orElse(parent.map(_(name))).get

  def contains(name: String): Boolean = nameToCode.contains(name) || parent.map(_.contains(name)).getOrElse(false)

  def toMap: Map[String, CodeGenNode] = 
    val parentMap = parent.map(_.toMap).getOrElse(Map())
    parentMap ++ nameToCode.toMap

  override def toString(): String = s"CodeEnv(${toMap})"