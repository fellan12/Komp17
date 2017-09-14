package punkt0
package analyzer


import Types.Typed


object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }



  sealed abstract class Symbol extends Positioned with Typed {

    val id: Int = ID.next
    val name: String
  }

  private object ID {

    private var c: Int = 1


    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()


    def lookupClass(n: String): Option[ClassSymbol] = {
      if (mainClass.name == n) {
        return Some(mainClass)
      } else {
        return classes.get(n)
      }

      return None

    }

  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()


    def lookupMethod(n: String): Option[MethodSymbol] = {
      if (methods.contains(n))
        return methods.get(n)
      else {
        parent match {
          case Some(p) => p.lookupMethod(n)
          case _ => None
        }
      }
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      if (members.contains(n)) {
        members.get(n)
      } else {
       
        
        parent match {
          // Behövs för att dubbelkolla att parentklassen inte har en variabel med samma namn
          // Class member overloading is not allowed
          case Some(p) => p.lookupVar(n)
          case _ => None 
        }
      }
    }

  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None


    def lookupVar(n: String): Option[VariableSymbol] = {
      if (params.contains(n)) {
        return params.get(n)
      } else if (members.contains(n)) {
        return members.get(n)
      } else {
        return None
      }

    }

  }

  class VariableSymbol(val name: String) extends Symbol


}

