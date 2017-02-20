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
       var reachedEOF = false;
       goForward;
       
       def goForward : Unit = {
         if (source.hasNext) {
			    source.next
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
        var current = source.ch;
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
        case ':' => COLON;       //:
        case ';' => SEMICOLON    //;
        case '.' => DOT          //.
        case ',' => COMMA        //,
        case '<' => LESSTHAN     // <
        case '+' => PLUS         // +
        case '-' => MINUS        // -
        case '*' => TIMES        // *
        case '!' => BANG         // !
        case '(' => LPAREN       // (
        case ')' => RPAREN       // )
        case '[' => LBRACE       // {
        case ']' => RBRACE       // }
        
        case '=' => ???
         
        
        case '&' => ???
        
        case '|' => ???
        
        case '/' => ???
          
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
