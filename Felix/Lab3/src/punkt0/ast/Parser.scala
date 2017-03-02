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
     * Declarations
     *****************************************************************************************************************
     *****************************************************************************************************************/
    
    /*
     * Parse program Declaration
     */
    def programDecl: Program = {
      ??? //TODO
    }
    
    /*
     * Parse class Declaration
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
     * Parse Var Declaration
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
			
			eat(SEMICOLON)
			
      var retTree = new VarDecl(tpe,id, expression)
			retTree.setPos(currentToken)
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
    
    /**************************************************************************************************************
     **************************************************************************************************************
     * Expressions
     **************************************************************************************************************
     **************************************************************************************************************/
    
    /*
     * Parse Expression
     * 
		 * expr 		::= or ors
		 * or				::= and ands
		 * ors			::= E | || or ors
		 * and			::= cmp cmps
		 * ands     ::= E | && and ands
		 * cmp			::= term terms
		 * cmps			::= E | < cmp cmps | == cmp cmps
		 * term 		::= factor factors
		 * terms 		::= E | + term terms | [expr] terms
		 * factor 	::= ! factor | unary unaries
		 * factors 	::= E | * factor factors
		 * unary		::= {terminals} | id followsId | new followsNew |  ...
		 * unaries	::= E | . followsDot unaries
		 *
     */
    def expression: ExprTree = ors(or)
    
    /*
		 * Parse Ors
		 */
		def ors(t: ExprTree): ExprTree = currentToken.kind match {
			case OR => eat(OR); ors(Or(t, or))
			case _ => t
		}
    
    /*
		 * Parse Or
		 * or				::= and ands
		 */
		def or: ExprTree = ands(and)
		
		 /*
     * Parse Ands
     */
		def ands(t: ExprTree): ExprTree = currentToken.kind match {
			case AND => eat(AND); ands(And(t, and))
			case _ => t
		}
		
		/*
		 * Parse And
		 * and			::= cmp cmps
		 */
		def and: ExprTree = cmps(cmp)
		
		/*
		 * Parse Comperators
		 */
		def cmps(t: ExprTree): ExprTree = currentToken.kind match {
      case LESSTHAN => eat(LESSTHAN); cmps(LessThan(t, cmp))
			case EQUALS => eat(EQUALS); cmps(Equals(t, cmp))
			case _ => t		
		}
		
		/*
     * Parse Compare
		 * cmp			::= term terms
     */
		def cmp: ExprTree = terms(term)
		
		/*
		 * Parse Terms
		 */
		def terms(t: ExprTree): ExprTree = {
		  ??? //TODO
		}
		
     /*
     * Parse Terms
		 * term 		::= factor factors
     */
    def term: ExprTree = factors(factor)
    
    /*
		 * Parse Factors
		 */
		def factors(t: ExprTree): ExprTree = {
		  ??? //TODO
		}
    
    /*
     * Parse Factor
     */
    def factor: ExprTree = currentToken.kind match {
			case BANG => {
				var startToken = currentToken
				eat(BANG)
				var newFactor = factor
				new Not(newFactor).setPos(startToken)
			}
			case _ => unaries(unary)
		}
    
    /*
		 * Parse Unaries
		 */
		def unaries(t: ExprTree): ExprTree = {
		  ??? //TODO
		}
		
		/*
		 * Parse Unary
		 */
		def unary: ExprTree = {
		  ??? //TODO
		  //TODO
		}
		
		/*****************************************************************************************************************
		 *****************************************************************************************************************
		 * TYPE and IDENTIFIERS
		 *****************************************************************************************************************
		 *****************************************************************************************************************/
		
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
    val tree = programDecl
    terminateIfErrors
    tree
  }
}
