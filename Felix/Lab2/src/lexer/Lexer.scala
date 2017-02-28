package punkt0
package lexer

import java.io.File
import java.lang.Boolean
import java.util.ArrayList;



object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._
  var bugging = false
  
  def debug(str: String): Unit = {
    if (bugging) {
      println(str)
    }
  }
  
  def printcurr(curr: Char) : Unit = {
    if (bugging) {
      if (curr == '\n') {
        println("current is newline")
      }
      else {
        println("Current: " + curr)
      }
    }
  }

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    // TODO: implement this method
    debug("run function starts")
    

    return new Iterator[Token] {
       var current = source.ch
       debug("start current:" + current)
       var reachedEOF = false
       var EOFPrinted = false
       var errorDetected = false
       goForward;
       printcurr(current)
       
       def goForward : Unit = {
         debug("går in i goforw")
         if (source.hasNext) {
           debug("char finns")
			    current = source.next
			     debug("Current är nu: " + current)
  		   } else {
  		      debug("reachedEOF")
  			    reachedEOF = true
  			 }
       }
      
      /*
       * Tokens left
       */
      def hasNext  = {
        debug("checks if next")
        if(EOFPrinted && errorDetected){
          terminateIfErrors()
        }
        !EOFPrinted  //END OF FILE  
        
      }

      /*
       * Return next token
       */
      def next : Token = {
       var token: Token = new Token(BAD)
       var token_position = source.pos

       				
				if (reachedEOF) {
					token = new Token(EOF)
					token.setPos(f, source.pos)
					EOFPrinted = true
					return token
				}

				// Skip whitespace
				while (!reachedEOF && current.isWhitespace) {
					goForward
					return next
				}


				// ID or keyword
				if (current.isLetter) { // must start with a letter
					debug("current var bokstav")
          var str = "";          //Write as StringBuilder?
          while(!reachedEOF && (current.isLetterOrDigit || current == '_')){
            debug("bygger sträng med: " + current)
            str += current
            goForward
          }
          debug("sträng blev: " + str)
          debug("checkar om ID eller Keyword")
          var tkRes = match_ID_or_Keyword(str)
          
          if(tkRes == IDKIND ){
            debug("token var ID")
            token = new ID(str)
            token.setPos(f, token_position)
          }else{
            debug("token var keyword")
            token = new Token(tkRes)
            token.setPos(f,token_position)
          }
				}
				// Int literal
				else if (current.isDigit) {
					if (current == '0') {
						goForward
						token = new INTLIT(0) // no leading zeros
					} else {
						var counter = 0;
						while(!reachedEOF && current.isDigit) {
							counter = 10*counter + current.asDigit
							goForward
						}
						token = new INTLIT(counter)
					}
				}
				
				// String literal
				else if(current == '"') {
					val b = new StringBuffer
					goForward // skip leading '"'

					while (!reachedEOF && current != '"') {
						if (current == '\n') {
							error("String literal cannot contain a newline character.", token.setPos(f, source.pos))
						}
						b.append(current)
						goForward
					}

					if(current == '"') {
						goForward //Skip the trailing '"'
						token = new STRLIT(b.toString)
					} else {
						error("String literal not closed with \".", token.setPos(f, source.pos))
					}
				}

				//Handle symbols
				else {
          var oneMoreStep = true
					current match {

						//Braces, parenthesis and brackets
						case '(' => token = new Token(LPAREN)
						case ')' => token = new Token(RPAREN)
						case '{' => token = new Token(LBRACE)
						case '}' => token = new Token(RBRACE)

						//Arithmetic operators (except /)
						case '+' => token = new Token(PLUS)
						case '-' => token = new Token(MINUS)
						case '*' => token = new Token(TIMES)

						//Punctuation symbols
						case '.' => token = new Token(DOT)
						case ',' => token = new Token(COMMA)
						case ';' => token = new Token(SEMICOLON)
						case ':' => token = new Token(COLON)
						case '!' => token = new Token(BANG)

						//Single- and multiline comments or DIV operator
						case '/' => {
							goForward

							// Single line comment
							if (!reachedEOF && current == '/') {
								// skip forward to the next line
								while (!reachedEOF && current != '\n') { goForward }
								return next
							}

							// Multiline block comment
							else if (!reachedEOF && current == '*') {

								try {
									while (source.next != '*' || source.next != '/') {}

									} catch {
										case nsee: NoSuchElementException => fatal("Unclosed block comment.", token.setPos(f, source.pos))
									}


								goForward // move past the trailing '/'
								return next
							}
							else {
								oneMoreStep = false
								token = new Token(DIV)
							}
						}

						// < or EXTENDS
						case '<' => {
							goForward
							if (!reachedEOF && current == ':') {
								token = new Token(EXTENDS)
							}
							else {
								token = new Token(LESSTHAN)
								oneMoreStep = false
							}
						}

						// = and ==
						case '=' => {
							goForward
							if (!reachedEOF && current == '=') {
								token = new Token(EQUALS)
							}
							else {
								token = new Token(EQSIGN)
								oneMoreStep = false
							}
						}

						//Logical operators
						case '|' => {
							goForward
							if (!reachedEOF && current == '|')
								token = new Token(OR)
							else
								fatal("Illegal 'OR' operator syntax.", token.setPos(f, source.pos))
						}
						case '&' => {
							goForward
							if (!reachedEOF && current == '&')
								token = new Token(AND)
							else
								fatal("Illegal 'AND' operator syntax.", token.setPos(f, source.pos))
						}

						//Unexpected symbols
						case _ => {
								fatal("Unrecognized symbol.", token.setPos(f, token_position))
							}
						}

						if (oneMoreStep) {
						  goForward
						}
					}

					token.setPos(f, token_position)
					token
				}
 
      /*
       * Return ID/Keyword TokenKind of tok     
       */
      def match_ID_or_Keyword(tok : String) : TokenKind = tok match {
        case  "object" => OBJECT       // 
        case  "class" => CLASS        // class
        case  "def" => DEF          // def
        case  "override" => OVERRIDE     // override
        case  "var" => VAR          // var
        case  "Unit" => UNIT         // Unit
        case  "String" => STRING       // String
        case  "extends" => EXTENDS      // extends
        case  "Int" => INT          // Int
        case  "Boolean" => BOOLEAN      // Boolean
        case  "while" => WHILE        // while
        case  "if" => IF           // if
        case  "else" => ELSE         // else
        case  "true" => TRUE         // true
        case  "false" => FALSE        // false
        case  "this" => THIS         // this
        case  "null" => NULL         // null
        case  "new" => NEW          // new
        case  "println" => PRINTLN      // println
        case  _ => IDKIND;
        
      }
    }
  }
}
