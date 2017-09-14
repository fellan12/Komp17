package punkt0
package ast

import Trees._
import scala.collection.mutable.ListBuffer
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  // Helper class for unpacking values of the kind "X(value)"
  implicit class UnpackValues(val str: String) {
    def unpack(tpe: String): Option[String] = {
      val r = """%s\((.*)\)""".format(tpe).r
      str match {
        case r(value) => Some(value)
        case _ => None
      }
    }
  }
  
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._

    /** Stores the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    /** Reads a token **/
    def readToken : Unit = {
      if (tokens.hasNext) {
        // uses next from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** "Eats" the expected token, otherwise it terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*) = {
      error("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    /*****************************************************************************************************************
     *****************************************************************************************************************
     * Declarations - Program, ClassDeclaration, MainDeclaration, VarDeclaration, ************************************
     ***************  MethodDeclaration, Types, Expressions, Identifiers *********************************************
     *****************************************************************************************************************/
    
    /*
     * Parse Program:
     * Program ::= ( ClassDeclaration ) * MainDeclaration
     */
    def program: Program = {
      var pos = currentToken
      
      // Parse class/-es
      var classes = new ListBuffer[ClassDecl]()
      while (currentToken.kind == CLASS){
        // Get all classes from program
        classes += classDecl
      }
      
      // Parse main declaration
      var mainMethod = mainDecl
      
      eat(EOF)
      var p : Program = new Program(mainMethod, classes.toList)
      p.setPos(pos)
      return p
    }
    
    /*
     * Parse Class Declaration:
     * ClassDeclaration ::= class Identifier ( extends Identifier )? { ( VarDeclaration ) * ( MethodDeclaration ) * }
     */
    def classDecl : ClassDecl = {
      var pos = currentToken
      
      eat(CLASS)
      
      // Parse identifier of class
      var id = identifier                       
      
      // Parent
      var parent: Option[Identifier] = None
      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        pos = currentToken
        // Parse parent identifier ("Extends")
        parent = Some(identifier)
        parent.get.setPos(pos)             
      }
      
      eat(LBRACE)
      
      // Variables
      var varsList = new ListBuffer[VarDecl]
      while (currentToken.kind == VAR){
        // Parse all variables in class
        varsList += varDecl          
      }
      
      // Methods
      var methodList = new ListBuffer[MethodDecl]
      while(currentToken.kind == OVERRIDE || currentToken.kind == DEF){ // method may start with override??
        // Parse all methods in class
        methodList += methodDecl    
      }
      
      eat(RBRACE)
  
      var retTree = new ClassDecl(id,parent, varsList.toList, methodList.toList)
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Main Declaration:
     * Main declaration ::= object Identifier extends Identifier { ( VarDeclaration ) * Expression ( ; Expression ) * }
     */
    def mainDecl : MainDecl = {
      var pos = currentToken
      
      eat(OBJECT)
      
      // Parse main identifier of main declaration
      var mainID = identifier
    
      eat(EXTENDS)
      
      // Parse parent identifier of main declaration
      var parentID = identifier

      eat(LBRACE)
    
      // Variables
      var varList = new ListBuffer[VarDecl]
      while (currentToken.kind == VAR){
        // Parse all variables in main declaration
        varList += varDecl          
      }
      
      // Expressions
      var exprList = new ListBuffer[ExprTree]
      
      // Always parse one expression in main declaration
      exprList += expression
      while(currentToken.kind == SEMICOLON){
        // Parse rest of the expressions in main declaration
        eat(SEMICOLON)
        exprList += expression
      }
      
      eat(RBRACE)
      
      var retTree = new MainDecl(mainID, parentID, varList.toList, exprList.toList)
      retTree.setPos(pos)
      retTree      
    }
    
    /*
     * Parse Method Declaration
     * Method Declaration ::= ( override )? def Identifier ( ( Identifier : Type ( , Identifier : Type ) * )? ) : Type = { ( VarDeclaration ) * Expression ( ; Expression ) * }
     */
    def methodDecl : MethodDecl = {
      var pos = currentToken
        
      // Override check
      var overRide = false
      if(currentToken.kind == OVERRIDE){
        overRide = true;
        eat(OVERRIDE)
      }
      
      eat(DEF)
      
      // Parse identifier of method declaration
      var id = identifier
      
      eat(LPAREN)
        
      // Arguments
      var argsList = new ListBuffer[Formal]
      if (currentToken.kind != RPAREN) {
        pos = currentToken
        // Identifier of argument
        var id = identifier
      
        eat(COLON)
          
        // Type of argument
        var tpe = typ
      
        // Add to list of arguments
        var f : Formal = new Formal(tpe, id)
        f.setPos(pos)
        argsList += f
        
        while (currentToken.kind != RPAREN) {
          if (currentToken.kind == COMMA){
            eat(COMMA) 
            pos = currentToken
            // Identifier of argument
            var id = identifier
      
            eat(COLON)
          
            // Type of argument
            var tpe = typ
      
            // Add to list of arguments
            var f2 : Formal = new Formal(tpe, id)
            f2.setPos(pos)
            argsList += f2
          }
        }
      }
     
  
      eat(RPAREN)
  
      eat(COLON)

      // Parse return type
      var returnType = typ

      eat(EQSIGN)

      // METHOD BODY
      eat(LBRACE)

      // Variables
      var varList = new ListBuffer[VarDecl]
      while (currentToken.kind == VAR){
        // Parse all variables in method declaration
        varList += varDecl          
      }

      // Expressions in method
      var exprList = new ListBuffer[ExprTree]
      // Always parse one expression in method declaration
      exprList += expression
      while(currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        // Parse all expressions in method declaration
        exprList += expression
      }
  
      eat(RBRACE)
    
      // Return expression is always the last one
      val retExpr = exprList.last

      // Rest of the expressions
      val regularExpr = exprList.dropRight(1)  

      var retTree = new MethodDecl(overRide,returnType, id, argsList.toList, varList.toList, regularExpr.toList, retExpr)
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Variable Declaration
     * Variable Declaration ::= var Identifier : Type = Expression ;
     */
    def varDecl : VarDecl = {
      var pos = currentToken
      
      eat(VAR)
      
      // Get identifier of variable
      var id = identifier
  
      eat(COLON)
      
      // Get type of variable
      var tpe = typ
      
      eat(EQSIGN)
      
      // Parse expression of variable
      var expr = expression
      
      eat(SEMICOLON)
      
      var retTree = new VarDecl(tpe,id, expr)
      retTree.setPos(pos)
      retTree   
  }
    
    /*
     * Parse Type
     * Type ::= Boolean || Int || String || Unit || Identifier
     */
    def typ: TypeTree = {
      var retTree: TypeTree = null
      var pos = currentToken
    
      // Check type kind
      currentToken.kind match {
        case BOOLEAN => eat(BOOLEAN); retTree = new BooleanType()
        case STRING => eat(STRING); retTree = new StringType()
        case UNIT => eat(UNIT); retTree = new UnitType()
        case INT => eat(INT); retTree = new IntType()
        case _ => retTree = identifier // type is an identifier
      }

      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Expression
     * Expression ::= Or
     */
    def expression: ExprTree = {  
      var pos = currentToken
      var retTree : ExprTree = or
      retTree.setPos(pos)
      retTree
    }
    /*
     * Parse Or
     * Or ::=  And || And (|| And)*
     */
    def or : ExprTree = {
      var pos = currentToken
      // Parse And
      var retTree : ExprTree = and
      
      while (currentToken.kind == OR) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        
        var lhs : ExprTree = retTree
        var rhs : ExprTree = and
        
        sign match {
          case OR => {
            retTree = new Or(lhs,rhs)
          }
          case _ => error("Expected OR",currentToken)

        }
      }
      retTree.setPos(pos)
      retTree
    }
    /*
     * Parse And
     * And ::= CompEqual && CompEqual (&& CompEqual)*
     */
    def and : ExprTree = {
      var pos = currentToken
      // Parse compequal
      var retTree : ExprTree = compequal
    
      while (currentToken.kind == AND) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        
        var lhs : ExprTree = retTree
        var rhs : ExprTree = compequal
      
        sign match {
          case AND => {
            retTree = new And(lhs,rhs)
          }
          case _ => error("Expected AND",currentToken)

        }
      }
      retTree.setPos(pos)
      retTree
    }
    /*
     * Parse CompEqual
     * CompEqual ::= Comp (<|==) Comp ( (<|==) Comp)*
     */
    def compequal : ExprTree = {
      var pos = currentToken
      // Parse comp
      var retTree : ExprTree = comp
   
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        
        var lhs : ExprTree = retTree
        var rhs : ExprTree = comp
        
        sign match {
          case LESSTHAN => {
            retTree = new LessThan(lhs,rhs)
          }
          case EQUALS => {
            retTree = new Equals(lhs,rhs)
          }
          case _ => error("Expected LESSTHAN or EQUALS",currentToken)

        }
      }
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Comp
     * Comp ::= Term (+|-) Term ( (+|-) Term)*
     */
    def comp : ExprTree = {
      var pos = currentToken
      // Parse term
      var retTree : ExprTree = term
      
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        var sign : TokenKind = currentToken.kind
        eat(sign) 
        
        var lhs : ExprTree = retTree
        var rhs : ExprTree = term
        
        sign match {
          case PLUS => {
            retTree = new Plus(lhs, rhs)
          }
          case MINUS => {
            retTree = new Minus(lhs, rhs)
          }
          case _ => error("Expected PLUS or MINUS",currentToken)

        }
      }
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Term
     * Term ::= Factor (*|/) Factor ( (*|/) Factor)*
     */
    def term : ExprTree = {
      var pos = currentToken
      // Parse bang
      var retTree : ExprTree = factor
     
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        
        var lhs : ExprTree = retTree
        var rhs : ExprTree = factor
        
        sign match {
          case TIMES => {
            retTree = new Times(lhs, rhs)
          }
          case DIV => {
            retTree = new Div(lhs, rhs)
          }
          case _ => error("Expected TIMES or DIV",currentToken)

        }
      }
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse Factor
     * Factor ::= (!)? MethodCall
     */
    def factor : ExprTree = {
      var pos = currentToken
      var retTree : ExprTree = null
      
      // Check if bang and parse method call
      if (currentToken.kind == BANG) {
        eat(BANG)
        retTree = new Not(methodCall)
      } else {
        retTree = methodCall
      }
      retTree.setPos(pos)
      retTree
    }
    
    /*
     * Parse MethodCall
     * MethodCall ::= Expr . Identifier ( Expression ( , Expression )* )
     */
    def methodCall : ExprTree = {
      var pos = currentToken
      // Parse expr
      var retTree : ExprTree = expr
    
      while ( currentToken.kind == DOT) {
        eat(DOT)
        
        // Parse identifier
        var id = identifier
    
        var exprList = new ListBuffer[ExprTree]
        var expr = retTree
     
        eat(LPAREN)
        
        if (currentToken.kind != RPAREN) {
          // Always parse one expression in method call
          exprList += expression
          while(currentToken.kind == COMMA){
            // Parse rest of the expressions in method call
            eat(COMMA)
            exprList += expression
          }
        }
      
        eat(RPAREN)
       
        retTree = new MethodCall(expr, id, exprList.toList)  
      }
      retTree.setPos(pos)   
      retTree
    }
    
    /*
     * Parse Expr ::=
     * Expr ::= println... | if (..) | while (..) | true | etc...
     */
    def expr : ExprTree = { 
      var pos = currentToken
      var retTree : ExprTree = null
      
      currentToken.kind match {
        // MethodCall ::= Println ( Expression )
        case PRINTLN => {
          eat(PRINTLN)
          eat(LPAREN)
          retTree = new Println(expression)
          eat(RPAREN)
          retTree.setPos(pos)
        }
        // Expr ::= <INTEGER_LITERAL>
        case INTLITKIND => {
          retTree = new IntLit(currentToken.toString.unpack("INT").get.toInt)
          eat(INTLITKIND)
          retTree.setPos(pos)
        }
        // Expr ::= " <STRING_LITERAL> "
        case STRLITKIND => {
          retTree = new StringLit(currentToken.toString.unpack("STR").get)
          eat(STRLITKIND)
          retTree.setPos(pos)
        }
        // Expr ::= true
        case TRUE => {       
          eat(TRUE)
          retTree = new True()
          retTree.setPos(pos)      
        }
        // Expr ::= false
        case FALSE => {       
          eat(FALSE)
          retTree = new False()
          retTree.setPos(pos)        
        }
        // Expr ::= this
        case THIS => {        
          eat(THIS)
          retTree = new This()
          retTree.setPos(pos)
        }
        // Epression ::= null
        case NULL => {
          eat(NULL)
          retTree = new Null()
          retTree.setPos(pos)        
        }
        // Expr ::= New Identifier ()
        case NEW => {
          eat(NEW)
          // Parse identifier
          var id = identifier
          eat(LPAREN)
          eat(RPAREN)
          retTree = new New(id)
          retTree.setPos(pos)   
        }
        // Expr ::= ( Expression )
        case LPAREN => {
          eat(LPAREN)
          retTree = expression
          eat(RPAREN)
          retTree.setPos(pos)
        }
        // Expr ::= { ( Expression ( ; Expression ) * )? }
        case LBRACE => {       
          eat(LBRACE)
          // Expressions
          var exprList = new ListBuffer[ExprTree]
          if (currentToken.kind != RBRACE) {
             // Always parse one expression in block
            exprList += expression
            while(currentToken.kind == SEMICOLON) {
              // Parse rest of expressions in block
              eat(SEMICOLON)
              exprList += expression
            }
          }
          eat(RBRACE)
          retTree = new Block(exprList.toList)
          retTree.setPos(pos)
        }
        // Expr ::= if ( Expression ) Expression ( else Expression )?
        case IF => {
          eat(IF)
          eat(LPAREN)
          // Parse if condition
          var ifRet = expression
          eat(RPAREN)
          // Parse if body
          var thenRet = expression
          if(currentToken.kind == ELSE) {
            eat(ELSE)
            // Parse else body
            var elseRet : Option[ExprTree] = Some(expression)
            retTree = new If(ifRet, thenRet, elseRet)
          }else {
            retTree = new If(ifRet, thenRet, None)
          }
          retTree.setPos(pos)
        }
        // Expr ::= while ( Expression ) Expression
        case WHILE => {
          eat(WHILE)       
          eat(LPAREN)
          // Parse while condition
          var condRet = expression        
          eat(RPAREN)
          // Parse while body         
          var bodyRet = expression
          retTree = new While(condRet,bodyRet)
          retTree.setPos(pos)
        }
        // Expr ::= IdentOrAssign
        case IDKIND => {    
          retTree = identOrAssign
          retTree.setPos(pos)
        }
        case _ => {
          fatal("Invalid KEYWORD or ID", currentToken)
        }       
      }
      retTree
    }
      
    /*
     * Parse Identifier
     * Identifier ::= <IDENTIFIER>
     */
  def identifier: Identifier = {
    var pos = currentToken
    var id = currentToken  
    eat(IDKIND)
    try{
      var i :Identifier = new Identifier(id.toString.unpack("ID").get)
      i.setPos(pos)
      return i
    } catch {
      case _ : NoSuchElementException => {
        fatal("Could not extract identifier", pos);
      }
    }
  }
  
  /*
   * Parse IdentOrAssign
   * IdentOrAssign ::= Identifier | Identifier = Expression
   */
  def identOrAssign: ExprTree = {
    var pos = currentToken
    var id : Identifier = new Identifier(currentToken.toString.unpack("ID").get)
    id.setPos(pos)

    eat(IDKIND)
  
    if(currentToken.kind == EQSIGN) {
      // IdentOrAssign ::= Identifier = Expression
      eat(EQSIGN)
  
      var as : Assign = new Assign(id,expression)
      as.setPos(pos)
      return as
    } else {
      // IdentOrAssign ::= Identifier
      return id
    }
  }

    readToken
    val tree = program
    terminateIfErrors
    tree
  }
}