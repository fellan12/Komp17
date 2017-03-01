package punkt0
package ast

import Trees._
import scala.collection.mutable.ListBuffer
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  
  // Helper class for unpacking values of the kind "X(value)"
	implicit class UnpackValues(val s: String) {
		def unpack(tpe: String): Option[String] = {
			val r = """%s\((.*)\)""".format(tpe).r
			s match {
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

    /*
     * Parse program Declaration
     */
    def program: Program = {
      ??? //TODO
    }
    
    /*
     * Parse class Declaration
     */
    def classDecl : ClassDecl = {
      ??? //TODO
    }
    
    /*
     * Parse Var Declaration
     */
    def varDecl : VarDecl = {
      var pos = currentToken
			eat(VAR)
			var id = identifier
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
      ??? //TODO
    }
    
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
     * Parse Terms
     */
    def term: ExprTree = factors(factor)
    
    /*
     * Parse Compare
     */
		def cmp: ExprTree = terms(term)
		
		/*
		 * Parse And
		 */
		def and: ExprTree = cmps(cmp)
		
		/*
		 * Parse Or
		 */
		def or: ExprTree = ands(and)
		
		/*
		 * Parse Ors
		 */
		def ors(t: ExprTree): ExprTree = currentToken.kind match {
			case OR => eat(OR); ors(Or(t, or))
			case _ => t
		}
		
    /*
     * Parse Ands
     */
		def ands(t: ExprTree): ExprTree = currentToken.kind match {
			case AND => eat(AND); ands(And(t, and))
			case _ => t
		}

		/*
		 * Parse Comperators
		 */
		def cmps(t: ExprTree): ExprTree = currentToken.kind match {
      case LESSTHAN => eat(LESSTHAN); cmps(LessThan(t, cmp))
			case EQUALS => eat(EQUALS); cmps(Equals(t, cmp))
			case _ => t		}
		
		/*
		 * Parse Unary
		 */
		def unary: ExprTree = {
		  ??? //TODO
		  //TODO
		}
		
		/*
		 * Parse Unaries
		 */
		def unaries(t: ExprTree): ExprTree = {
		  ??? //TODO
		}
		
		/*
		 * Parse Terms
		 */
		def terms(t: ExprTree): ExprTree = {
		  ??? //TODO
		}
		
		/*
		 * Parse Factors
		 */
		def factors(t: ExprTree): ExprTree = {
		  ??? //TODO
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
