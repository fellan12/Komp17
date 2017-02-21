package punkt0
package lexer

import java.io.File
import java.lang.Boolean
import java.util.ArrayList;


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    // TODO: implement this method

    return new Iterator[Token] {
       var current = source.ch;
       var reachedEOF = false;
       goForward;
       
       def goForward : Unit = {
         if (source.hasNext) {
			    current = source.next
  		   } else {
  			    reachedEOF = true
  			 }
       }
      
      /*
       * Tokens left?
       */
      def hasNext = {
          !reachedEOF;      //END OF FILE
      }

      /*
       * Return next token
       */
      def next : Token = {
        var token : Token = new Token(BAD);
        var token_position = source.pos;

        
        if(reachedEOF){
          token = new Token(EOF);
          token.setPos(f, source.pos);
          return token;
        }
        
        // Skip whitespace
        while(current.isWhitespace){
          goForward;
          return next;
        }
        
        //ID and Keyword
        if(current.isLetter){    //Start of ID or Keyword
          var str = "";          //Write as StringBuilder?
          while(!reachedEOF && (current.isLetterOrDigit || current == '_')){
            str += current;
            goForward;
          }
          
          var tkRes = match_ID_or_Keyword(str);
          
          if(tkRes == IDKIND ){
            token = new ID(str)
            token.setPos(f, token_position)
          }else{
            token = new Token(tkRes);
            token.setPos(f,token_position);
          }
        }
        //Int Literals
        else if(current.isDigit){
          token = new INTLIT(0);
          while(current == '0'){
            goForward;
          }
          if(current.isDigit){
            var counter = 0;
            while(!reachedEOF && current.isDigit){
              counter += 10*counter + current.asDigit;
              goForward;
            }
            token = new INTLIT(counter);
            token.setPos(f, token_position);
          }
        }
        //String Literal
        else if(current == '"'){
          goForward;
          var str = "";
          while(!reachedEOF && current != '"'){
            if(current == '\n'){
              fatal("String can't contians newLines", token.setPos(f, token_position));
            }
            str += current;
            goForward;
          }
          
          if(current == '"'){
            token = new STRLIT(str);
            token.setPos(f, token_position);
            goForward;
          }
        }
        //Comments or DIV
        else if (current == '/') {
          goForward;
          println(current);
          if(current == '/') {
            // comment en rad
            println(current);
            while (!reachedEOF && current != '\n') {
              goForward;
              println(current);
              //next;
            }
            goForward;
            //next;
            //println(current);
           
          }
          else if(current == '*') {
            // comment fram till */
            var prev = current;
            goForward;
            while (!reachedEOF && prev != '*' && current != '/') {
              prev = current;
              goForward;
            }
            //goForward;
            next;
              
          }
          else {
            token = new Token(DIV)
            token.setPos(f, token_position)
          }
        }
        //Symbols
        else{
          var tkRes = match_Symbols(current);
          
          token = new Token(tkRes);
          token.setPos(f, token_position);
          
        }
        
        return token;
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
