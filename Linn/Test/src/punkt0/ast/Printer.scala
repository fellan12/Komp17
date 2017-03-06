package punkt0
package ast

import Trees._

object Printer {
  var res : String = null
  var treestr : String = " ";
  
  def digest : String = {
    var wrd = ""
    while (treestr.charAt(0) != '(' || treestr.charAt(0) != ',' || treestr.charAt(0) != ')') {
      wrd += treestr.charAt(0)
      treestr = treestr.substring(1)
    }
    
    while(treestr.charAt(0) == '(' || treestr.charAt(0) == ',' || treestr.charAt(0) == ')' ){
      treestr = treestr.substring(1)
    }
      
    wrd
  }
  
  def apply(t: Tree): String = {
    var treestr = t.toString
    
    while (!treestr.isEmpty()) {
      var currentword = digest
      currentword match {
        case "Program" => 
        case "MainMethod" => {
          mainmethod
        }
        
      }
    }
    
    res
  }
  
  def mainMethod() {
    res += "object "
    //Äta upp "Identifier("
    digest
    
    res += digest  //MainID
    res += " extends "
    res += digest  //ParentID
    res += " {\n"
    
    while (digest != "VarDecl") {}
    while (digest == "Vardecl") {
      res += varDecl()      //Vardecl
      
    }
    digest // tar bort parametern innan expr
    // fixa expression
  }
  
  def varDecl() : String = {
   // while(digest != "VarDecl"){}
    digest match {
      case "StringType" => var typ = "String"
      case "IntType" => var typ = "Int"
      case "BooleanType" => var typ = "Boolean"
      case "UnitType" => var typ = "Unit"
    }
    
    //eat Identifier
    digest
    var id = digest      //Variable name
    
    //eat literal
    var value = digest  //Variable value
    return "var " + id + " : " + typ + " = " + value;
  }
}
