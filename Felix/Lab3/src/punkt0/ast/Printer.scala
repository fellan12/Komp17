package punkt0
package ast

import Trees._
import lexer._
import scala.collection.mutable.StringBuilder

object Printer {
  var indent = "  ";

  def apply(tree: Tree): String = {
    val codeBuilder = new StringBuilder

    tree match {
      case prog: Program => {
        prog.classes.foreach(                         
            clas => codeBuilder.append((apply(clas)))
        )
        codeBuilder.append(apply(prog.main))
      }
      
      case clas: ClassDecl => {
        codeBuilder.append(strRep(CLASS) + " ")
        codeBuilder.append(clas.id.value)
        clas.parent match {
          case Some(p) => codeBuilder.append(" " + strRep(EXTENDS) + " " + p.value)
          case _ => "Unknown Parent"
        }
        codeBuilder.append( " {\n")
        clas.vars.foreach(                           
            v => codeBuilder.append(indent + apply(v))
        )
        clas.methods.foreach(                         
            m => codeBuilder.append(indent +apply(m))
        )
        codeBuilder.append( "}\n")
      }
      
      case varDec: VarDecl => {
        codeBuilder.append(indent + strRep(VAR) + " " +  varDec.id.value + " : " + apply(varDec.tpe) + " = " + apply(varDec.expr) + ";\n")
      }
      
      case mainMet: MainMethod => {
         codeBuilder.append(strRep(OBJECT) + " " + mainMet.main.id.value)
        if (!mainMet.main.args.isEmpty) {
          codeBuilder.append((apply(mainMet.main.args.head)))
        }
        mainMet.main.args.drop(1).foreach(                              
            a => codeBuilder.append(", " + apply(a))
        )
        codeBuilder.append( " extends " + apply(mainMet.parent) + " {\n")

        mainMet.main.vars.foreach(
            v => codeBuilder.append(apply(v))
        )
        if (!mainMet.main.exprs.isEmpty) {
          codeBuilder.append(apply(mainMet.main.exprs.head) + ";")
        }
        mainMet.main.exprs.drop(1).foreach(                             
            e => codeBuilder.append(apply(e) + ";\n")
        )
        codeBuilder.append(apply(mainMet.main.retExpr))
        codeBuilder.append( "\n}\n")
      }
      
      //MAIN  --- USE LATER
      case mainDecl: MainDecl => {
         codeBuilder.append(strRep(OBJECT) + " " + mainDecl.obj.value)
        
        codeBuilder.append( " extends " + mainDecl.parent.value +  " = {\n")

        mainDecl.vars.foreach(
            v => codeBuilder.append(apply(v))
        )
        
        if (!mainDecl.exprs.isEmpty) {
          codeBuilder.append(apply(mainDecl.exprs.head) + ";")
        }
        var lastExpr = mainDecl.exprs.last;
        mainDecl.exprs.drop(1).dropRight(1).foreach(                             
            e => codeBuilder.append(apply(e) + ";\n")
        )
        codeBuilder.append(apply(lastExpr))

        codeBuilder.append( "\n}\n")
      }
      
      case method: MethodDecl => {
        if(method.overrides == true){
          codeBuilder.append(strRep(OVERRIDE) + " ")
        }
        codeBuilder.append(strRep(DEF) + " " + method.id.value + "(")
        if (!method.args.isEmpty) {
          codeBuilder.append((apply(method.args.head)))
        }
        method.args.drop(1).foreach(                              
            a => codeBuilder.append(", " + apply(a))
        )
        codeBuilder.append( ") : " + apply(method.retType) + " = {\n")
        // BODY
        method.vars.foreach(
            v => codeBuilder.append(apply(v))
        )
        if (!method.exprs.isEmpty) {
          codeBuilder.append(indent+ apply(method.exprs.head) + ";\n")
        }
        //continue on drop(1)
        method.exprs.drop(1).foreach(                             
            e => codeBuilder.append(apply(e) + ";\n")
        )
        codeBuilder.append(apply(method.retExpr))
        codeBuilder.append( "\n}\n")
      }
      
      case id: Identifier => {
        codeBuilder.append(id.value)
      }
      
      case formal: Formal => {
        codeBuilder.append(apply(formal.id)+ " : " + apply(formal.tpe))
      }
      
      case tpe: TypeTree => {
        tpe match {
          case _: IntType => codeBuilder.append( "Int")
          case _: StringType =>  codeBuilder.append( "String")
          case _: BooleanType => codeBuilder.append( "Boolean")
          case _: UnitType => codeBuilder.append( "Unit")
          case _ => "Unknown Type"
        }
      }
      
      case expr: ExprTree => expr match {
        case and: And => codeBuilder.append(apply(and.lhs) + " " + strRep(AND) + " " + apply(and.rhs))
        case or: Or => codeBuilder.append(apply(or.lhs) + " " + strRep(OR) + " " + apply(or.rhs))
        case plus: Plus => codeBuilder.append("("+ apply(plus.lhs) + " " + strRep(PLUS) + " " + apply(plus.rhs) + ")")
        case minus: Minus => codeBuilder.append("("+ apply(minus.lhs) + " " + strRep(MINUS) + " " + apply(minus.rhs) + ")")
        case times: Times => codeBuilder.append("("+apply(times.lhs) + " " + strRep(TIMES) + " " + apply(times.rhs)+ ")")
        case div: Div => codeBuilder.append("("+apply(div.lhs) + " " + strRep(DIV) + " " + apply(div.rhs)+ ")")
        case lessthan: LessThan => codeBuilder.append(apply(lessthan.lhs) + " " + strRep(LESSTHAN) + " " + apply(lessthan.rhs))
        case eq: Equals => codeBuilder.append(apply(eq.lhs) + " " + strRep(EQUALS) + " " + apply(eq.rhs))
          
        case int: IntLit => codeBuilder.append(int.value)
        case str: StringLit => codeBuilder.append("\"" + str.value + "\"")

        case tru: True => codeBuilder.append(true)
        case fals: False => codeBuilder.append(false)
        case nul: Null => codeBuilder.append("null")

        case ne: New => codeBuilder.append(strRep(NEW) + " " +apply(ne.tpe)+"()")
        case no: Not => codeBuilder.append(strRep(BANG) + "(" + apply(no.expr) + ")")

        case block: Block => {
          codeBuilder.append("{\n")
          if (!block.exprs.isEmpty) {
            codeBuilder.append(indent + apply(block.exprs.head))
          }
          //continue on drop(1)
          block.exprs.drop(1).foreach(                             
              e => codeBuilder.append(";\n" + apply(e))
          )
          codeBuilder.append("\n" + indent + "}")
        }
        case i: If => {
          codeBuilder.append(indent + strRep(IF) + " (" + apply(i.expr) + ")\n " + apply(i.thn) + "\n ")
          if(i.els.isDefined){
          codeBuilder.append(indent + strRep(ELSE) + "\n " + apply(i.els.get))
          }  
        }
        case whil: While => codeBuilder.append(indent + strRep(WHILE) + " (" + apply(whil.cond) + ") " + apply(whil.body))
        case print: Println => codeBuilder.append(indent + strRep(PRINTLN) + "(" + apply(print.expr) + ")")
        case assign: Assign => codeBuilder.append(apply(assign.id) +" " + strRep(EQSIGN) + " " + apply(assign.expr))
        case methCall: MethodCall => {
          codeBuilder.append( apply(methCall.obj) + strRep(DOT) + apply(methCall.meth) + "(")
          if (!methCall.args.isEmpty) {
          codeBuilder.append((apply(methCall.args.head)))
          }
          methCall.args.drop(1).foreach(                              
            a => codeBuilder.append(", " + apply(a))
          )
          codeBuilder.append(")")
        }
        case thi: This => codeBuilder.append(strRep(THIS))
        case _ => "Unknown Expr"
      }
      case _ => "Unknown Declaration"
    }

    codeBuilder.toString
  }
  
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
        case THIS => "this"
        case OVERRIDE => "override"
        case _ => "Unknownn String Representation"
    }
}
