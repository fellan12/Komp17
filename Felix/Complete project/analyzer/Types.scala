package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  //Type - Error
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  //Type - Untyped
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  //Type - Int
  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }

  //Type - Bool
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Bool"
  }

  //Type - String
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }

  //Type - Unit
  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"
  }

  //Type - Null
  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyRef(_) => true
      case TNull => true
      //case TString => true
      case _ => false
    }

  }

 /* //Type - Class
  case class TClass(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyRef(_) => true
      case TClass(superclsymbol) => {
        if (superclsymbol == classSymbol) { return true }
        var parentsymbol = classSymbol.parent
        while(parentsymbol.isDefined) {
          if (parentsymbol.get == superclsymbol) { return true }
          parentsymbol = parentsymbol.get.parent
        }
        return false
      }
      case _ => false
    }

    override def toString = classSymbol.name
  }
*/
  // special object to implement the fact that all objects are its subclasses
  //val anyClass = TClass(new ClassSymbol("AnyClass"))

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      if(tpe == classSymbol.getType || tpe == anyRef) {
        return true
      } else if (classSymbol.parent.isDefined) {
        return classSymbol.parent.get.getType.isSubTypeOf(tpe)
      } else {
        return false
      }

      tpe match {
        case TAnyRef(_) => true
        case _ => false
      }
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}

