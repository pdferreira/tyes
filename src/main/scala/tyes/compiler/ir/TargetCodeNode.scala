package tyes.compiler.ir

enum TargetCodeNode:
  case If(cond: TargetCodeNode, thenBranch: TargetCodeNode, elseBranch: TargetCodeNode)
  case For(cursors: Seq[TargetCodeForCursor], body: TargetCodeNode)
  case Throw(excClass: TargetCodeTypeRef, excMessage: TargetCodeNode)
  case Try(tryBody: TargetCodeNode, excClass: TargetCodeTypeRef, catchBody: TargetCodeNode)
  case Text(str: String)
  case FormattedText(formattingStr: (String | TargetCodeNode)*)
  case Integer(n: Int)
  case Boolean(b: scala.Boolean)
  case Unit
  case Var(name: String)
  case Field(obj: TargetCodeNode, field: String)
  case Not(exp: TargetCodeNode)
  case Equals(left: TargetCodeNode, right: TargetCodeNode)
  case NotEquals(left: TargetCodeNode, right: TargetCodeNode)
  case And(left: TargetCodeNode, right: TargetCodeNode)
  case Or(left: TargetCodeNode, right: TargetCodeNode)
  case Apply(fun: TargetCodeNode, args: TargetCodeNode*)
  case Let(varName: String, varExp: TargetCodeNode, bodyExp: TargetCodeNode)
  case Match(matchedExp: TargetCodeNode, branches: Seq[(TargetCodeNode, TargetCodeNode)])
  case Return(exp: TargetCodeNode)

class TargetCodeTypeRef private(
  val name: String, 
  val namespaces: Seq[String], 
  val params: Seq[TargetCodeTypeRef]
):
  def copy(name: String = this.name, namespaces: Seq[String] = this.namespaces, params: Seq[TargetCodeTypeRef] = this.params) =
    new TargetCodeTypeRef(name, namespaces, params)

object TargetCodeTypeRef:
  def apply(name: String, params: TargetCodeTypeRef*) = new TargetCodeTypeRef(name, Seq(), params)
  def apply(qualifiedName: String*) = 
    val ns :+ name = qualifiedName
    new TargetCodeTypeRef(name, ns, Seq())

enum TargetCodeForCursor:
  case Iterate(name: String, collection: TargetCodeNode)
  case Let(name: String, exp: TargetCodeNode)
  case Filter(exp: TargetCodeNode)

case class TargetCodeUnit(name: String, decls: Seq[TargetCodeDecl])

enum TargetCodeDecl:
  case Type(alias: String, typeRef: TargetCodeTypeRef)
  case Method(name: String, params: Seq[(String, TargetCodeTypeRef)], retTypeRef: TargetCodeTypeRef, body: TargetCodeNode)
  case Import(namespaces: Seq[String], all: scala.Boolean = false)
  case Class(name: String, inherits: Seq[TargetCodeTypeRef], decls: Seq[TargetCodeDecl])
  case ADT(name: String, inherits: Seq[TargetCodeTypeRef], constructors: Seq[TargetCodeADTConstructor])

case class TargetCodeADTConstructor(
  name: String,
  params: Seq[(String, TargetCodeTypeRef)] = Seq(),
  inherits: Seq[TargetCodeBaseTypeCall] = Seq()
)

case class TargetCodeBaseTypeCall(typeRef: TargetCodeTypeRef, args: TargetCodeNode*)