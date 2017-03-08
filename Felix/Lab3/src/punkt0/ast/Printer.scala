package punkt0
package ast

import Trees._
import lexer._
import scala.collection.mutable.StringBuilder

object Printer {
  var indent = "   ";
  def strRep(x: TokenKind): String = x match {
        case LBRACE => "{"
        case RBRACE => "}"
        case LPAREN => "("
        case RPAREN => ")"
        case EQSIGN => "="
        case EQUALS => "=="
        case AND => "&&"
        case OR => "||"
        case BANG => "!"
        case DOT => "."
        case COMMA => ","
        case COLON => ":"
        case SEMICOLON => ";"
        case PLUS => "+"
        case MINUS => "-"
        case DIV => "/"
        case TIMES => "*"
        case LESSTHAN => "<"
        case CLASS => "class"
        case EXTENDS => "extends"
        case VAR => "var"
        case OBJECT => "object"
        case DEF => "def"
        case NEW => "new"
        case IF => "if"
        case ELSE => "else"
        case WHILE => "while"
        case PRINTLN => "println"
        case _ => "Unknownn String Representation"
    }

  def apply(tree: Tree): String = {
    val stringRep = new StringBuilder

    tree match {
      case prog: Program => {
        prog.classes.foreach(                         
            clas => stringRep.append((apply(clas)))
        )
        stringRep.append(apply(prog.main))
      }
      
      case clas: ClassDecl => {
        stringRep.append(strRep(CLASS))
        stringRep.append("" + clas.id.value)
        clas.parent match {
          case Some(p) => stringRep.append(strRep(EXTENDS) + " " + p.value)
        }
        stringRep.append( " {\n")
        clas.vars.foreach(                           
            v => stringRep.append(apply(v))
        )
        clas.methods.foreach(                         
            m => stringRep.append(apply(m))
        )
        stringRep.append( "}\n")
      }
      
      case varDec: VarDecl => {
        stringRep.append(indent + strRep(VAR) + " " +  varDec.id.value + " : " + apply(varDec.tpe) + " = " + apply(varDec.expr) + "\n")
      }
      
      case mainMet: MainMethod => {
         stringRep.append(strRep(OBJECT) + " " + mainMet.main.id.value)
        if (!mainMet.main.args.isEmpty) {
          stringRep.append((apply(mainMet.main.args.head)))
        }
        mainMet.main.args.foreach(                              
            a => stringRep.append(", " + apply(a))
        )
        stringRep.append( " : " + apply(mainMet.main.retType) + " = {\n")

        mainMet.main.vars.foreach(
            v => stringRep.append(apply(v))
        )
        if (!mainMet.main.exprs.isEmpty) {
          stringRep.append(apply(mainMet.main.exprs.head) + ";")
        }
        mainMet.main.exprs.foreach(                             
            e => stringRep.append(apply(e) + ";\n")
        )
        stringRep.append(apply(mainMet.main.retExpr))
        stringRep.append( "\n}\n")
      }
      
      case method: MethodDecl => {
        stringRep.append(strRep(DEF) + " " + method.id.value)
        if (!method.args.isEmpty) {
          stringRep.append((apply(method.args.head)))
        }
        method.args.foreach(                              
            a => stringRep.append(", " + apply(a))
        )
        stringRep.append( ") : " + apply(method.retType) + " = {\n")
        // BODY
        method.vars.foreach(
            v => stringRep.append(apply(v))
        )
        if (!method.exprs.isEmpty) {
          stringRep.append(apply(method.exprs.head) + ";")
        }
        //continue on tail
        method.exprs.tail.foreach(                             
            e => stringRep.append(apply(e) + ";\n")
        )
        stringRep.append(apply(method.retExpr))
        stringRep.append( "\n}\n")
      }
      
      case id: Identifier => {
        stringRep.append(id.value)
      }
      
      case formal: Formal => {
        stringRep.append(apply(formal.id)+ " : " + apply(formal.tpe))
      }
      
      case tpe: TypeTree => {
        tpe match {
          case _: IntType => stringRep.append( "Int")
          case _: StringType =>  stringRep.append( "String")
          case _: BooleanType => stringRep.append( "Bool")
          case _: UnitType => stringRep.append( "Unit")
          case _ => "Unknown Type"
        }
      }
      
      case expr: ExprTree => expr match {
        case and: And => stringRep.append(apply(and.lhs) + " " + strRep(AND) + " " + apply(and.rhs))
        case or: Or => stringRep.append(apply(or.lhs) + " " + strRep(OR) + " " + apply(or.rhs))
        case plus: Plus => stringRep.append(apply(plus.lhs) + " " + strRep(PLUS) + " " + apply(plus.rhs))
        case minus: Minus => stringRep.append(apply(minus.lhs) + " " + strRep(MINUS) + " " + apply(minus.rhs))
        case times: Times => stringRep.append(apply(times.lhs) + " " + strRep(TIMES) + " " + apply(times.rhs))
        case div: Div => stringRep.append(apply(div.lhs) + " " + strRep(DIV) + " " + apply(div.rhs))
        case lessthan: LessThan => stringRep.append(apply(lessthan.lhs) + " " + strRep(LESSTHAN) + " " + apply(lessthan.rhs))
        case eq: Equals => stringRep.append(apply(eq.lhs) + " " + strRep(EQUALS) + " " + apply(eq.rhs))
          
        case int: IntLit => stringRep.append(int.value)
        case str: StringLit => stringRep.append("\"" + str.value + "\"")

        case tru: True => stringRep.append(true)
        case fals: False => stringRep.append(false)

        case ne: New => stringRep.append(strRep(NEW) + " " +apply(ne.tpe)+"()")
        case no: Not => stringRep.append(strRep(BANG) + "(" + apply(no.expr) + ")")

        case block: Block => {
          stringRep.append("{\n")
          if (!block.exprs.isEmpty) {
            stringRep.append(indent + apply(block.exprs.head))
          }
          //continue on tail
          block.exprs.tail.foreach(                             
              e => stringRep.append(";\n" + apply(e))
          )
          stringRep.append("\n" + indent + "}")
        }
        case i: If => {
          stringRep.append(indent + strRep(IF) + " (" + apply(i.expr) + ") " + apply(i.thn))
          if(i.els.isDefined){
          stringRep.append(indent + strRep(ELSE) + " " + apply(i.els.get))
          }  
        }
        case whil: While => stringRep.append(indent + strRep(WHILE) + " (" + apply(whil.cond) + ") " + apply(whil.body))
        case print: Println => stringRep.append(indent + strRep(PRINTLN) + "(" + apply(print.expr) + ")")
        case assign: Assign => stringRep.append(indent + apply(assign.id) +" " + strRep(EQSIGN) + " " + apply(assign.expr))
        case _ => "Unknown Expr"
      }
      case _ => "Unknown Declaration"
    }

    stringRep.toString
  }
}
