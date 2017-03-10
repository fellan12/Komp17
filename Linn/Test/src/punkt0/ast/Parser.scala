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
    	
    	//var 
			return new Program(mainMethod, classes.toList).setPos(currentToken)
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
			
      // Variables
			eat(LBRACE)
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
     * Parse Main declaration
     */
    def mainDecl : MainDecl = {
     
      var pos = currentToken  
      eat(OBJECT)
      //ID
     
      var mainID = identifier
    
      //ID OF PARENT
      eat(EXTENDS)
      var parentID = identifier

      
      
      eat(LBRACE)
    
       //Variables in method
    	var varList = new ListBuffer[VarDecl]
    	while (currentToken.kind == VAR){
    	  //Get all variables in class
    	  varList += varDecl          
    	}
      
    	//Expressions in method
    	var exprList = new ListBuffer[ExprTree]
    	
    	//Always parse for one expression
    	exprList += expression
    	while(currentToken.kind == SEMICOLON){
    	  eat(SEMICOLON)
    	  exprList += expression
    	}
      
      /*
      // vardecl ??? followed by expression ??
      //MAIN METHOD BODY
      var method = methodDecl
		
      *
      */
    	
    	var retTree = new MainDecl(mainID, parentID, varList.toList, exprList.toList)
    	eat(RBRACE)

      retTree.setPos(pos)
      
      retTree      
    }
    
    /*
     * Parse Method Declaration
     */
    def methodDecl : MethodDecl = {
	    var pos = currentToken
	      
	    //Override check
	    var overRide = false
	    if(currentToken.kind == OVERRIDE){
	    	overRide = true;
	        eat(OVERRIDE)
	    }
	    eat(DEF)
	    
	   
	    //ID of method
	    
	    var id = identifier
	      
	    eat(LPAREN)
	      
	    //Arguments
	    var argsList = new ListBuffer[Formal]
	    var firstArg = true
	  
		while (currentToken.kind != RPAREN) {
		
			if (currentToken.kind == COMMA){
			  eat(COMMA) 
		  } else {
		    //Identifier of argument
			  var id = identifier
			
	
			  eat(COLON)
			  //Type of argument
			  var tpe = typ
			
			  //Add to list of Arguments
			  argsList += new Formal(tpe, id)
			}
		}
		
	
		eat(RPAREN)
	
		eat(COLON)

		//Return type
		var returnType = typ

		eat(EQSIGN)

		/////METHOD BODY
		eat(LBRACE)

		//Variables in method

		var varList = new ListBuffer[VarDecl]
		while (currentToken.kind == VAR){
		
		  //Get all variables in class
		  varList += varDecl          
		}

		//Expressions in method
		var exprList = new ListBuffer[ExprTree]
		//Always parse for one expression
		exprList += expression
		while(currentToken.kind == SEMICOLON){
		  eat(SEMICOLON)
		  exprList += expression
		}
		
	
		eat(RBRACE)
		//Return expression is always the last one
		val retExpr = exprList.last

		//Rest of the expressions
		val regularExpr = exprList.dropRight(1)  

	    var retTree = new MethodDecl(overRide,returnType, id, argsList.toList, varList.toList, regularExpr.toList, retExpr)
	    retTree.setPos(pos)
	    retTree
    }
    
    /*
     * Parse Variable Declaration
     */
    def varDecl : VarDecl = {
   
      var pos = currentToken
			eat(VAR)
      
      //ID
      //Get ID of variable
			var id = identifier
	
			
			eat(COLON)
			//Type
			//Get Type of var
			var tpe = typ
			
			eat(EQSIGN)
      
      //EXPRESSION
      //Get expression of the variable
      var expr = expression
			
			eat(SEMICOLON)
			
      var retTree = new VarDecl(tpe,id, expr)
			retTree.setPos(currentToken)
			retTree   
	}
    
    /*
	 * Parse Type
	 */
	def typ: TypeTree = {
	
	  var retTree: TypeTree = null
		var pos = currentToken
		
		
	  /*
	   * Which kind of Type is it
	   */
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
	 */
    def expression: ExprTree = {
    
       var retTree : ExprTree = or
     
     
      retTree.setPos(currentToken)
 
      retTree
    }
    def or : ExprTree = {
   
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
    
    
    
    def and : ExprTree = {
     
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
    def compequal : ExprTree = {
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
    
    def comp : ExprTree = {
   
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
    
    
    def term : ExprTree = {
      
      var retTree : ExprTree = bang
     
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        var lhs : ExprTree = retTree
        var rhs : ExprTree = bang
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
    
    def bang : ExprTree = {
    
      var retTree : ExprTree = null
      if (currentToken.kind == BANG) {
    
        eat(BANG)
        retTree = new Not(expr)
      
      }
      else {
        retTree = expr
      }
      
      retTree
    }
    
    def expr : ExprTree = {
    
      var retTree : ExprTree = factor
    

      while ( currentToken.kind == DOT) {
        // id
        eat(DOT)
        var id = identifier
    
        var exprList = new ListBuffer[ExprTree]
        var expr = retTree
     
        eat(LPAREN)
        
        if (currentToken.kind != RPAREN) {
          //Expressions in method
		      //Always parse for one expression
          
		      exprList += expression
		      while(currentToken.kind == COMMA){
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
    
    
    
    def factor : ExprTree = {
 
      
      var retTree : ExprTree = null
      currentToken.kind match {
        
        case PRINTLN => {
    
          eat(PRINTLN)
          eat(LPAREN)
          retTree = new Println(expression)
          eat(RPAREN)
          retTree.setPos(currentToken)
         
        }
        case INTLITKIND => {
       
          retTree = new IntLit(currentToken.toString.unpack("INTLITKIND").get.toInt)
          retTree.setPos(currentToken)
          eat(INTLITKIND)
     
        }
        case STRLITKIND => {
          retTree = new StringLit(currentToken.toString.unpack("STRLITKIND").get)
          retTree.setPos(currentToken)
          eat(STRLITKIND)
        
        }
        case TRUE => {
       
          eat(TRUE)
          retTree = new True()
          retTree.setPos(currentToken)
      
        }
        case FALSE => {
         
          eat(FALSE)
          retTree = new False()
          retTree.setPos(currentToken)
         
        }
        case THIS => {
          
          eat(THIS)
          retTree = new This()
          retTree.setPos(currentToken)
        }
        case NULL => {
          eat(NULL)
          retTree = new Null()
          retTree.setPos(currentToken)
          
        }
        case NEW => {
          eat(NEW)
          var id = identifier
          eat(LPAREN)
          eat(RPAREN)
          retTree = new New(id)
          retTree.setPos(currentToken)
        
        }
      
        case LPAREN => {
        
          eat(LPAREN)
          retTree = expression
         
          eat(RPAREN)
          retTree.setPos(currentToken)
   
        }
        case LBRACE => {
        
          eat(LBRACE)
          //Expressions in method
          var exprList = new ListBuffer[ExprTree]
          if (currentToken.kind != RBRACE) {
            
		         //Always parse for one expression
		        exprList += expression
		        while(currentToken.kind == SEMICOLON){
		    
		          eat(SEMICOLON)
		          exprList += expression
		        }
          }
          eat(RBRACE)
          retTree = new Block(exprList.toList)
          retTree.setPos(currentToken)
        }
        case IF => {
      
          eat(IF)
       
          eat(LPAREN)
      
          var ifRet = expression
          eat(RPAREN)
          var thenRet = expression
          if(currentToken.kind == ELSE) {
            eat(ELSE)
            var elseRet : Option[ExprTree] = Some(expression)
            retTree = new If(ifRet, thenRet, elseRet)
          }else {
            retTree = new If(ifRet, thenRet, None)
          }
          retTree.setPos(currentToken)
        }
        case WHILE => {
         
          eat(WHILE)
         
          eat(LPAREN)
        
          var condRet = expression
        
          eat(RPAREN)
          
          
          var bodyRet = expression
         
          
          retTree = new While(condRet,bodyRet)
          retTree.setPos(currentToken)
        }
        case IDKIND => {
    
          retTree = identOrAssign
          retTree.setPos(currentToken)
        }
        case _ => {
          error("Expression failed")
        }
        
      }

      retTree
    }
    	
		/*
		 * Parse Identifier
		 */
	def identifier: Identifier = {

	  var id : Identifier = null

		try {
		  /*
		   * Current token should be, so just unpack it
		   */
			id = new Identifier(currentToken.toString.unpack("IDKIND").get)
		}
		catch {
			case nsee: NoSuchElementException => error("Unable to extract identifier", currentToken)
		}

		id.setPos(currentToken)
		eat(IDKIND)
		return id
	}
	
	def identOrAssign: ExprTree = {
	
	  var id = new Identifier(currentToken.toString.unpack("IDKIND").get)

	  eat(IDKIND)
	
	  if(currentToken.kind == EQSIGN){
	  
	    eat(EQSIGN)
	
	    new Assign(id,expression)
	  }else{
	
	    id
	  }
	}
	
	

    readToken
    val tree = program
    terminateIfErrors
    tree
  }
}
