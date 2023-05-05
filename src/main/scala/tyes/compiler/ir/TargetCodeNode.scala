package tyes.compiler.ir

enum TargetCodeNode:
  case If(cond: TargetCodeNode, thenBranch: TargetCodeNode, elseBranch: TargetCodeNode)
  case For(cursors: Seq[TargetCodeForCursor], body: TargetCodeNode)
  case Throw(excClass: String, excMessage: TargetCodeNode)
  case Try(tryBody: TargetCodeNode, excClass: String, catchBody: TargetCodeNode)
  case Text(str: String)
  case FormattedText(formattingStr: (String | TargetCodeNode)*)
  case Integer(n: Int)
  case Boolean(b: scala.Boolean)
  case Unit
  case Var(str: String)
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

enum TargetCodeForCursor:
  case Iterate(name: String, collection: TargetCodeNode)
  case Let(name: String, exp: TargetCodeNode)
  case Filter(exp: TargetCodeNode)
