package tyes.compiler.ir

enum CodeGenNode:
  case If(cond: CodeGenNode, thenBranch: CodeGenNode, elseBranch: CodeGenNode)
  case For(cursors: Seq[CodeGenForCursor], body: CodeGenNode)
  case Throw(excClass: String, excMessage: CodeGenNode)
  case Try(tryBody: CodeGenNode, excClass: String, catchBody: CodeGenNode)
  case Text(str: String)
  case FormattedText(formattingStr: (String | CodeGenNode)*)
  case Integer(n: Int)
  case Boolean(b: scala.Boolean)
  case Unit
  case Var(str: String)
  case Field(obj: CodeGenNode, field: String)
  case Not(exp: CodeGenNode)
  case Equals(left: CodeGenNode, right: CodeGenNode)
  case NotEquals(left: CodeGenNode, right: CodeGenNode)
  case And(left: CodeGenNode, right: CodeGenNode)
  case Or(left: CodeGenNode, right: CodeGenNode)
  case Apply(fun: CodeGenNode, args: CodeGenNode*)
  case Let(varName: String, varExp: CodeGenNode, bodyExp: CodeGenNode)
  case Match(matchedExp: CodeGenNode, branches: Seq[(CodeGenNode, CodeGenNode)])
  case Return(exp: CodeGenNode)

enum CodeGenForCursor:
  case Iterate(name: String, collection: CodeGenNode)
  case Let(name: String, exp: CodeGenNode)
  case Filter(exp: CodeGenNode)
