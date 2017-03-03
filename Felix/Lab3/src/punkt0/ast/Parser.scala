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
     * Declarations - Program, Class, 
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
			while(currentToken.kind == DEF){
			  //Get all methods in class
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
    def mainDecl : MainMethod = {
      var pos = currentToken  
      eat(OBJECT)
      //ID
      var mainID = identifier
            
      //ID OF PARENT
      eat(EXTENDS)
      var parentID = identifier
      
      eat(LBRACE)
      
      //MAIN METHOD BODY
      var method = methodDecl
			
			eat(RBRACE)
      
      var retTree = new MainMethod(mainID, parentID, method)
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
			eat(COLON)                // type declaration starts always with ":"
			
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
      ??? //TODO
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
				id = new Identifier(currentToken.toString.unpack("ID").get)
			}
			catch {
				case nsee: NoSuchElementException => error("Unable to extract identifier", currentToken)
			}

			id.setPos(currentToken)
			eat(IDKIND)
			return id
		}

    readToken
    val tree = program
    terminateIfErrors
    tree
  }
}
