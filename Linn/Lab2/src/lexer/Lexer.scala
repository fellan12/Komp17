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
       * Tokens left?
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
        debug("går in i next")
        var token : Token = new Token(BAD);
        var token_position = source.pos;

        
        if(reachedEOF) {
          debug("checks if reachedEOF")
          token = new Token(EOF)
          token.setPos(f, source.pos)
          debug("returns EOF")
          EOFPrinted = true;
          return token
        }
        
        // Skip whitespace
        while(current.isWhitespace){
          debug("skips whitespace")
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
              //next;
            }
            debug("slut på while")
            return next
            //goForward
            //goForward
            //next;
            //println(current);
           
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
            
            //goForward;
            return next
          }
          else {
            token = new Token(DIV)
            token.setPos(f, token_position)
            return token
          }
        }
          
        
        //ID and Keyword
        if (current.isLetter && !reachedEOF) {    //Start of ID or Keyword
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
        
        //Int Literals
        else if(current.isDigit){
          debug("Current var siffra")
          token = new INTLIT(0);
          while(current == '0'){
            debug("hoppa över nollor")
            goForward;
          }
          if(current.isDigit) {
            var counter = 0;
            while(!reachedEOF && current.isDigit){
              debug("bygger tal med: " + current)
              counter += 10*counter + current.asDigit;
              goForward;
            }
            debug("tal blev: " + counter)
            token = new INTLIT(counter);
            token.setPos(f, token_position);
          }
        }
        //String Literal
        else if(current == '"'){
          debug("current var sträng")
          goForward;
          var str = "";
          while(!reachedEOF && current != '"'){
            if(current == '\n'){
              error("String can't contains newLines", token.setPos(f, token_position));
            }
            debug("bygger sträng med: " + current)
            str += current;
            goForward;
          }
          debug("sträng blev: " + str)
          
          if(current == '"'){
            debug("sträng är slut")
            token = new STRLIT(str);
            token.setPos(f, token_position);
            goForward;
          }
        }
      
       
        
        //Symbols
        else{
          debug("current var övrig/symbol")
          printcurr(current)
          var tkRes = match_Symbols(current);
          debug("" + hasNext)
          token = new Token(tkRes);
          token.setPos(f, token_position);
          goForward;
          
        }
        if (token.toString == "BAD") {
          errorDetected = true
          error("Bad Token Detected", token.setPos(f, token_position))
        }
        return token
      }
      
      /*
       * Return Symbol TokenKind of Tok
       */
      def match_Symbols(tok : Char) : TokenKind = tok match {
        case ':' => goForward; COLON  //:
        case ';' => goForward; SEMICOLON    //;
        case '.' => goForward; DOT         //.
        case ',' => goForward; COMMA        //,
        case '<' => goForward; LESSTHAN     // <
        case '+' => goForward; PLUS         // +
        case '-' => goForward; MINUS        // -
        case '*' => goForward; TIMES        // *
        case '!' => goForward; BANG         // !
        case '(' => goForward; LPAREN       // (
        case ')' => goForward; RPAREN       // )
        case '[' => goForward; LBRACE       // {
        case ']' => goForward; RBRACE       // }
        
        case '=' => goForward;
          // current = "="
          if (source.ch == '=') {
            goForward;
            return EQUALS
          } else {
            return EQSIGN
          }
         
        
        case '&' => goForward;
           // current = "&"
          if (source.ch == '&') {
            goForward;
            return AND
          } else {
            return BAD  //fix?
          }
        
        case '|' => goForward;
             // current = "|"
          if (source.ch == '|') {
            goForward;
            return OR
          } else {
            return BAD  //fix?
          }
          
        case _ => BAD;       
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
