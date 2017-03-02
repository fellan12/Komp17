package punkt0
package lexer

import java.io.File


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
       
       def goForward = {
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
       var token = new Token(BAD)
       var token_position = source.pos

       				
				if (reachedEOF) {
					token = new Token(EOF)
					token.setPos(f, source.pos)
					EOFPrinted = true
					return token
				}

				// Skip whitespace
				if (!reachedEOF && current.isWhitespace) {
					goForward
					return next
				}
				
				
				   //Removes comment if it exists, otherwise returns DIV token
        if (current == '/') {
          debug("current var /")
          goForward;
          debug("Nästa är: " + current)
          if(current == '/') {
            // comment en rad
            debug("kommentar //")
            debug("går till while-loop")
            while (!reachedEOF && current != '\n') {
              goForward
              printcurr(current)
            }
            debug("slut på while")
            return next
           
          }
          else if(current == '*') {
            // comment fram till */
            debug("kommentar /*")
            var prev = current;
            goForward;
           try {
              while (!reachedEOF && current != '*' && source.next != '/') {
              }
            }
            catch {
              case nsee: NoSuchElementException => 
                error("Block comment not closed", token.setPos(f, token_position))
            }
            
            return next
          }
          else {
            token = new Token(DIV)
            token.setPos(f, token_position)
            return token
          }
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
          token = new Token(match_Symbols(current, token))
				}
				
					token.setPos(f, token_position)
					token
				}
      
      def match_Symbols(current : Char, token: Token) : TokenKind = {
        var oneMoreStep = true;
        var tok : TokenKind = current match {

						//Braces, parenthesis and brackets
						case '(' => LPAREN
						case ')' => RPAREN
						case '{' => LBRACE
						case '}' => RBRACE

						//Arithmetic operators (except /)
						case '+' => PLUS
						case '-' => MINUS
						case '*' => TIMES

						//Punctuation symbols
						case '.' => DOT
						case ',' => COMMA
						case ';' => SEMICOLON
						case ':' => COLON
						case '!' => BANG
						case '<' => LESSTHAN
						
						// = and ==
						case '=' => {
							goForward
							if (!reachedEOF && source.ch == '=') {
								EQUALS
							}
							else {
								oneMoreStep = false
								EQSIGN
							}
						}

						//Logical operators
						case '|' => {
							goForward
							if (!reachedEOF && source.ch == '|')
								OR
							else
								error("Illegal 'OR' operator syntax.", token.setPos(f, source.pos))
								BAD
						}
						case '&' => {
							goForward
							if (!reachedEOF && source.ch == '&')
								AND
							else
								error("Illegal 'AND' operator syntax.", token.setPos(f, source.pos))
								BAD
						}

						//Unexpected symbols
						case _ => {
							error("Unrecognized symbol.", token.setPos(f, source.pos))
							BAD
							}
						} 
        
        if (oneMoreStep) {
					goForward
				}
        return tok
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
