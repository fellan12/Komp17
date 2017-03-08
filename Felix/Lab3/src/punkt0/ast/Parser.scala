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
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    var bugging = false;
    
    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
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
     * Declarations - Program, Class, Main, Method, Var, Type
     *****************************************************************************************************************
     *****************************************************************************************************************/
    
    /*
     * Parse Program
     */
    def program: Program = {
    	var pos = currentToken
		  
    	//CLASSES
    	var classes = new ListBuffer[ClassDecl]()
    	while (currentToken.kind == CLASS){
    	  //Get all classes from program
    	  classes += classDecl
    	}
			
    	//MAIN METHOD
    	var mainMethod = mainDecl
			
    	eat(EOF)
    	
			return new Program(mainMethod, classes.toList)
    }
    
    /*
     * Parse Class Declaration
     */
    def classDecl : ClassDecl = {
      var pos = currentToken
      eat(CLASS)
      
      //ID
      //Get ID of class
      var id = identifier                       
      
      //Parent
      var parent: Option[Identifier] = None
			if (currentToken.kind == EXTENDS) {
				eat(EXTENDS)
				//Get parent identifier (What is Extends)
				parent = Some(identifier)              
			}
			
      //VARS
			eat(LBRACE)
			var varsList = new ListBuffer[VarDecl]
			while (currentToken.kind == VAR){
			  //Get all variables in class
			  varsList += varDecl          
			}
			
			//METHODS
			var methodList = new ListBuffer[MethodDecl]
			while(currentToken.kind == OVERRIDE || currentToken.kind == DEF){ // method may start with override??
			  //Get all methods in class
			  methodList += methodDecl    
			}
			debug("endofclass")
		
			eat(RBRACE)
	
			
      var retTree = new ClassDecl(id,parent, varsList.toList, methodList.toList)
      retTree.setPos(pos)
      retTree
      
    }
    
    /*
     * Parse Main declaration
     */
    def mainDecl : MainMethod = {
      
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
    	//Return expression is always the last one
  		val retExpr = exprList.last
  
  		//Rest of the expressions
  		val regularExpr = exprList.dropRight(1)  
    	
    	var method = new MethodDecl(false, UnitType(),mainID, List(), varList.toList, regularExpr.toList, retExpr)
      var retTree = new MainMethod(mainID, parentID, method)
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
			  var tpe = typeDecl
			
			  //Add to list of Arguments
			  argsList += new Formal(tpe, id)
			}
		}
		
	
		eat(RPAREN)
	
		eat(COLON)

		//Return type
		var returnType = typeDecl

		eat(EQSIGN)

		/////METHOD BODY
		eat(LBRACE)

		//Variables in method
		debug("go through variables")
		var varList = new ListBuffer[VarDecl]
		while (currentToken.kind == VAR){
		  debug("more than one variable existed")
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
			var tpe = typeDecl
			
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
	def typeDecl: TypeTree = {
	  debug("enter type")
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
      debug("Enter EXPRESSION")
      var retTree : ExprTree = complogic
      //debug("still" + currentToken)
      retTree.setPos(currentToken)
      retTree
    }
    
    
    
    def complogic : ExprTree = {
      debug("enter COMPLOGIC")
      var retTree : ExprTree = compequal
      //println(currentToken)
      while (currentToken.kind == BANG || currentToken.kind == OR || currentToken.kind == AND) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        var lhs : ExprTree = retTree
        var rhs : ExprTree = compequal
        sign match {
          case BANG => {
          debug("operator was BANG")
          eat(BANG)
          retTree = new Not(expression)
          retTree.setPos(currentToken)
        }
          case OR => {
            debug("operator was OR")
            retTree = new Or(lhs,rhs)
          }
          case AND => {
            debug("operator was AND")
            retTree = new And(lhs,rhs)
          }
        }
      }
      //println(currentToken)
      retTree.setPos(currentToken)
      retTree
    }
    def compequal : ExprTree = {
      debug("enter COMPEQUAL")
      var retTree : ExprTree = comp
      
      
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        var lhs : ExprTree = retTree
        var rhs : ExprTree = comp
        
        sign match {
          case LESSTHAN => {
            debug("operator was LESSTHAN")
            retTree = new LessThan(lhs,rhs)
          }
          case EQUALS => {
            debug("operator was EQUALS")
            retTree = new Equals(lhs,rhs)
          }
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    def comp : ExprTree = {
      debug("enter COMP")
      var retTree : ExprTree = term
  
    
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        var sign : TokenKind = currentToken.kind
        eat(sign)        
        var lhs : ExprTree = retTree
    
        var rhs : ExprTree = term
        sign match {
          case PLUS => {
            debug("operator was PLUS")
            retTree = new Plus(lhs, rhs)
          }
          case MINUS => {
            debug("operator was MINUS")
            retTree = new Minus(lhs, rhs)
          }
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    
    def term : ExprTree = {
      debug("enter TERM")
      var retTree : ExprTree = expr

      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        var sign : TokenKind = currentToken.kind
        eat(sign)
        var lhs : ExprTree = retTree
        var rhs : ExprTree = expr
        sign match {
          case TIMES => {
            debug("operator was TIMES")
            retTree = new Times(lhs, rhs)
          }
          case DIV => {
            debug("operator was DIV")
            retTree = new Div(lhs, rhs)
          }
        }
      }
      retTree.setPos(currentToken)
      retTree
    }
    
    def expr : ExprTree = {
      debug("enter EXPR")
      var retTree : ExprTree = factor
      //println(currentToken)

      while ( currentToken.kind == DOT) {
        debug("enter expr DOT")
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
      return retTree
    }
    
    def factor : ExprTree = {
      debug("enter FAKTOR")
      //println("token is " + currentToken)
      var retTree : ExprTree = null
      currentToken.kind match {
        
        case PRINTLN => {
          debug("Enter PRINTLN")
          eat(PRINTLN)
          eat(LPAREN)
          retTree = new Println(expression)
          eat(RPAREN)
          retTree.setPos(currentToken)
         
        }
        case INTLITKIND => {
          debug("Enter INTLITKIND")
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
          debug("enter TRUE")
          eat(TRUE)
          retTree = new True()
          retTree.setPos(currentToken)
      
        }
        case FALSE => {
          debug("enter FALSE")
          eat(FALSE)
           //println("hej" + currentToken)
          retTree = new False()
          retTree.setPos(currentToken)
         
        }
        case THIS => {
          debug("enter THIS")
          eat(THIS)
          retTree = new This()
          retTree.setPos(currentToken)
        }
        case NULL => {
          debug("enter NULL")
          eat(NULL)
          retTree = new Null()
          retTree.setPos(currentToken)
        }
        case NEW => {
          debug("enter NEW")
          eat(NEW)
          var id = identifier
          eat(LPAREN)
          eat(RPAREN)
          retTree = new New(id)
          retTree.setPos(currentToken)
        
        }
        case BANG => {
          debug("enter BANG")
          eat(BANG)
          retTree = new Not(expression)
          retTree.setPos(currentToken)
       
        }
        case LPAREN => {
          debug("enter LPAREN")
          eat(LPAREN)
          retTree = expression
          eat(RPAREN)
          retTree.setPos(currentToken)
   
        }
        case LBRACE => {
          debug("enter LBRACE")
          eat(LBRACE)
          //Expressions in method
          var exprList = new ListBuffer[ExprTree]
          if (currentToken.kind != RBRACE) {
            
		         //Always parse for one expression
		        exprList += expression
		        while(currentToken.kind == SEMICOLON){
		          debug("found semikolon")
		          eat(SEMICOLON)
		          exprList += expression
		        }
          }
          eat(RBRACE)
          retTree = new Block(exprList.toList)
          retTree.setPos(currentToken)
        }
        case IF => {
          debug("enter ID")
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
          debug("enter WHILE")
          eat(WHILE)
          eat(LPAREN)
          var condRet = expression
          eat(RPAREN)
          
          
          var bodyRet = expression
         
          
          retTree = new While(condRet,bodyRet)
          retTree.setPos(currentToken)
        }
        case IDKIND => {
          debug("enter IDKIND")
          retTree = identOrAssign
          retTree.setPos(currentToken)
        }
        case _ => {
          error("expr failed")
        }
        
      }
      
      retTree
    }
    	
		/*
		 * Parse Identifier
		 */
	def identifier: Identifier = {
	  debug("enter IDENTIFIER")
	  var id : Identifier = null

		try {
		  /*
		   * Current token should be, so just unpack it
		   */
			id = new Identifier(currentToken.toString.unpack("IDKIND").get)
			debug("Found ID:" + id)
		}
		catch {
			case nsee: NoSuchElementException => error("Unable to extract identifier", currentToken)
		}

		id.setPos(currentToken)
		eat(IDKIND)
		return id
	}
	
	def identOrAssign: ExprTree = {
	  debug("enter IDORASSIGN")
	  var id = new Identifier(currentToken.toString.unpack("IDKIND").get)

	  eat(IDKIND)
	
	  if(currentToken.kind == EQSIGN){
	    debug("Found ASSIGN for ID " + id)
	    eat(EQSIGN)
	
	    new Assign(id,expression)
	  }else{
	    debug("Found ID: " + id)
	    id
	  }
	}
	
	def debug(str : String) = {
	  if(bugging){
	    println(str)
	  }
	}

    readToken
    val tree = program
    terminateIfErrors
    tree
  }
}
