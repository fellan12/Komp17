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
    	
    	return new Program(mainMethod, classes.toList)
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
				// Parse parent identifier ("Extends")
				parent = Some(identifier)              
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
	      // Identifier of argument
			  var id = identifier
			
			  eat(COLON)
			    
			  // Type of argument
			  var tpe = typ
			
			  // Add to list of arguments
			  argsList += new Formal(tpe, id)
			  
			  while (currentToken.kind != RPAREN) {
		      if (currentToken.kind == COMMA){
			      eat(COMMA) 
		        // Identifier of argument
			      var id = identifier
			
			      eat(COLON)
			    
			      // Type of argument
			      var tpe = typ
			
			      // Add to list of arguments
			      argsList += new Formal(tpe, id)
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
			retTree.setPos(currentToken)
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

		  retTree.setPos(currentToken)
		  retTree
	  }
    
  	/*
  	 * Parse Expression
  	 * Expression ::= Or
  	 */
    def expression: ExprTree = {  
      var retTree : ExprTree = or
      retTree.setPos(currentToken)
      retTree
    }
    /*
     * Parse Or
     * Or ::=  And || And (|| And)*
     */
    def or : ExprTree = {
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
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    /*
     * Parse And
     * And ::= CompEqual && CompEqual (&& CompEqual)*
     */
    def and : ExprTree = {
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
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    /*
     * Parse CompEqual
     * CompEqual ::= Comp (<|==) Comp ( (<|==) Comp)*
     */
    def compequal : ExprTree = {
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
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    /*
     * Parse Comp
     * Comp ::= Term (+|-) Term ( (+|-) Term)*
     */
    def comp : ExprTree = {
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
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    /*
     * Parse Term
     * Term ::= Factor (*|/) Factor ( (*|/) Factor)*
     */
    def term : ExprTree = {
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
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    /*
     * Parse Factor
     * Factor ::= (!)? MethodCall
     */
    def factor : ExprTree = {
      var retTree : ExprTree = null
      
      // Check if bang and parse method call
      if (currentToken.kind == BANG) {
        eat(BANG)
        retTree = new Not(methodCall)
      } else {
        retTree = methodCall
      }
      retTree
    }
    
    /*
     * Parse MethodCall
     * MethodCall ::= Expr . Identifier ( Expression ( , Expression )* )
     */
    def methodCall : ExprTree = {
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
      retTree.setPos(currentToken)   
      retTree
    }
    
    /*
     * Parse Expr ::=
     * Expr ::= println... | if (..) | while (..) | true | etc...
     */
    def expr : ExprTree = { 
      var retTree : ExprTree = null
      
      currentToken.kind match {
        // MethodCall ::= Println ( Expression )
        case PRINTLN => {
          eat(PRINTLN)
          eat(LPAREN)
          retTree = new Println(expression)
          eat(RPAREN)
          retTree.setPos(currentToken)
        }
        // Expr ::= <INTEGER_LITERAL>
        case INTLITKIND => {
          retTree = new IntLit(currentToken.toString.unpack("INT").get.toInt)
          eat(INTLITKIND)
          retTree.setPos(currentToken)
        }
        // Expr ::= " <STRING_LITERAL> "
        case STRLITKIND => {
          retTree = new StringLit(currentToken.toString.unpack("STR").get)
          eat(STRLITKIND)
          retTree.setPos(currentToken)
        }
        // Expr ::= true
        case TRUE => {       
          eat(TRUE)
          retTree = new True()
          retTree.setPos(currentToken)      
        }
        // Expr ::= false
        case FALSE => {       
          eat(FALSE)
          retTree = new False()
          retTree.setPos(currentToken)        
        }
        // Expr ::= this
        case THIS => {        
          eat(THIS)
          retTree = new This()
          retTree.setPos(currentToken)
        }
        // Epression ::= null
        case NULL => {
          eat(NULL)
          retTree = new Null()
          retTree.setPos(currentToken)        
        }
        // Expr ::= New Identifier ()
        case NEW => {
          eat(NEW)
          // Parse identifier
          var id = identifier
          eat(LPAREN)
          eat(RPAREN)
          retTree = new New(id)
          retTree.setPos(currentToken)   
        }
        // Expr ::= ( Expression )
        case LPAREN => {
          eat(LPAREN)
          retTree = expression
          eat(RPAREN)
          retTree.setPos(currentToken)
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
          retTree.setPos(currentToken)
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
          retTree.setPos(currentToken)
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
          retTree.setPos(currentToken)
        }
        // Expr ::= IdentOrAssign
        case IDKIND => {    
          retTree = identOrAssign
          retTree.setPos(currentToken)
        }
        case _ => {
          fatal("Invalid Expression", currentToken)
        }       
      }
      retTree
    }
    	
		/*
		 * Parse Identifier
		 * Identifier ::= <IDENTIFIER>
		 */
	def identifier: Identifier = {
		var id = currentToken  
	  eat(IDKIND)
    try{
	    return new Identifier(id.toString.unpack("ID").get)
	  } catch {
	    case _ : NoSuchElementException => {
	      fatal("Could not extract identifier", currentToken);
	    }
	  }
	}
	
	/*
	 * Parse IdentOrAssign
	 * IdentOrAssign ::= Identifier | Identifier = Expression
	 */
	def identOrAssign: ExprTree = {
	
	  var id = new Identifier(currentToken.toString.unpack("ID").get)

	  eat(IDKIND)
	
	  if(currentToken.kind == EQSIGN) {
	    // IdentOrAssign ::= Identifier = Expression
	    eat(EQSIGN)
	
	    new Assign(id,expression)
	  } else {
	    // IdentOrAssign ::= Identifier
	    id
	  }
	}
    readToken
    val tree = program
    terminateIfErrors
    tree
  }
}
