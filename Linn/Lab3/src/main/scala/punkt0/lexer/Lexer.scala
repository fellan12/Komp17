package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._
  
  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    var bugging = false
    
    def debug(str: String): Unit = {
      if (bugging) {
        println(str)
      }
    }
   
    return new Iterator[Token] {
       var current = source.ch
       var reachedEOF = false
       var EOFPrinted = false
       var errorDetected = false
       goForward;
   
       /*
        * Moves one character forward
        */
       def goForward = {
         if (source.hasNext) {
			    current = source.next
  		   } else {
  			    reachedEOF = true
  			 }
       }
      
       /*
        * Checks if characters left
        */
       def hasNext  = {
        if(EOFPrinted && errorDetected){
          terminateIfErrors()
        }
        !EOFPrinted  //END OF FILE  
        
      }

       /*
        * Return next token
        */
        def next : Token = {
          debug("next startar med: " + current)
          debug("EOF" + reachedEOF)
	        var token = new Token(BAD)
	        var token_position = source.pos
       				
      		if (reachedEOF) {
      		  debug("setEOF")
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
      				
      		// Removes comment if it exists, otherwise returns DIV token
             	if (current == '/') {
             	    debug("found /")
               		goForward;
             	    debug("next is: " + current)
                		// One line comment
               		if(!reachedEOF && current == '/') {
                 			while (!reachedEOF && current != '\n') {
                   				goForward
                 			}
                 			return next
               		}
            	 	// Block comment
               		else if(!reachedEOF && current == '*') {
               		    debug("found block comment")
                
                 			goForward;
               		    debug("current: " + current)
             
                 			  var curr1 : String = current.toString()
                 			  goForward
                 			  var curr2 : String = current.toString()
                 			  var tmp : String = curr1 + curr2
                 			  while (!reachedEOF && tmp != "*/") {
                 			    curr1 = curr2.toString()
                 			    goForward
                 			    curr2 = current.toString()
                 			    tmp = curr1 + curr2
                 			    debug("tmp: " + tmp)
                 			  }
                 			  if (tmp != "*/") {
                 			    debug("COmment Not cloese")
                 			    error("Block comment not closed", token.setPos(f, token_position))
                 			  } else {
                 			    goForward
                 			  }

                   			

                 			
                 			debug("reachedEOF " + reachedEOF)
                 			debug("current: " + current)
                 			return next
               		}
               		// Division
               		else {
               		    
                 			token = new Token(DIV)
                 			token.setPos(f, token_position)
                 			return token
               		}
              
             	}
              
      		// ID or keyword
      		if (current.isLetter) { // must start with a letter
                 		var str = ""; 
                 		while(!reachedEOF && (current.isLetterOrDigit || current == '_')) {
                   			str += current
                   			goForward
                 		}
                 		var tkRes = match_ID_or_Keyword(str)
                
                 		if(tkRes == IDKIND ) {
                   			token = new ID(str)
                   			token.setPos(f, token_position)
                 		} else {
                   			token = new Token(tkRes)
              			 token.setPos(f, token_position)
                 		}
      		}
      				
      		// Int literal
      		else if (current.isDigit) {
      		  if(!reachedEOF && current == '0') {
      		      goForward
      		      token = new INTLIT(0)
      		  }
      		  else {
      		    var counter = 0;
      				while(!reachedEOF && current.isDigit) {
      					counter = 10*counter + current.asDigit
      					goForward
      				}
      				token = new INTLIT(counter)
      		  }
      
      		}
      				
      		 // String literal
      	 	else if (current == '"') {
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
      
      		// Handle symbols
      		else {
              token = new Token(match_Symbols(current, token))
      		}
      				
      		token.setPos(f, token_position)
      		token
	}
      
 /*
  * Tokenize a symbol and return it
  */
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
  			  debug("found =")
  				goForward
  				debug("current is: " + source.ch)
  				if (!reachedEOF && source.ch == '=') {
  					EQUALS
  				}
  				else {
  				  debug("next was not = ")
  					oneMoreStep = false
  					EQSIGN
  				}
  			}

  			//Logical operators
  			case '|' => {
  				goForward
  			  if (!reachedEOF && source.ch == '|') {
  			    OR
  			  }
  			  else {
  			    error("Illegal 'OR' operator syntax.", token.setPos(f, source.pos))
  			    oneMoreStep = false
  			    BAD
  			  } 
  			}
  			
  			case '&' => {
  				goForward
  				if (!reachedEOF && source.ch == '&') {
  					AND
  				}
  				else {
  					error("Illegal 'AND' operator syntax.", token.setPos(f, source.pos))
  					oneMoreStep = false
  					BAD
  				}
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
       * Tokenize ID/Keyword and return it
       */
        def match_ID_or_Keyword(tok : String) : TokenKind = tok match {
	        case  "object" => OBJECT       // 
	        case  "class" => CLASS         // class
	        case  "def" => DEF             // def
	        case  "override" => OVERRIDE   // override
	        case  "var" => VAR             // var
	        case  "Unit" => UNIT           // Unit
	        case  "String" => STRING       // String
	        case  "extends" => EXTENDS     // extends
	        case  "Int" => INT             // Int
	        case  "Boolean" => BOOLEAN     // Boolean
	        case  "while" => WHILE         // while
	        case  "if" => IF               // if
	        case  "else" => ELSE           // else
	        case  "true" => TRUE           // true
	        case  "false" => FALSE         // false
	        case  "this" => THIS           // this
	        case  "null" => NULL           // null
	        case  "new" => NEW             // new
	        case  "println" => PRINTLN     // println
	        case  _ => IDKIND;             // ID (Handle with care)
        
      	}
    }
  }
}
