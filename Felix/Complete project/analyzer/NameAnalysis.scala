package punkt0
package analyzer

import ast.Trees._
import Symbols._

import Types._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer

object NameAnalysis extends Phase[Program, Program] {




  var bugger = false
  def debug(str: String) = {
    if (bugger) {
      println(str)
    }
  }

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    debug("start of name analysis run")
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    var gs = new GlobalScope
    var mSym = new ClassSymbol(prog.main.obj.value)

   /* def phaseone(p: Program) {
      // Create symbol of main in global scope
      var mSym = mainSym()
      gs.mainClass = mSym

      // Create symbol of classes in global scope
      var classMap = Map[String, ClassSymbol]()
      p.classes.foreach(c => {
        classMap += (c.id.value -> clsSym())
        })
      gs.classes = classMap.toMap
    }

    def mainSym(main: MainDecl): ClassSymbol = {
      var mSym = new ClassSymbol(main.obj.value)

      // Parent in main should not have symbol

      // Set symbols of variables in main
      var varMap = Map[String, VariableSymbol]()
      main.vars.foreach(v => {
        varMap += (v.id.value -> varSym(v))
      })

      mSym.members = varMap.toMap

      // Set symbols of expressions in main
      mn.exprs.foreach(e => {
        exprSym(e)
      })

      // Set symbol of MainDecl and its name
      mn.setSymbol(mSym).setPos(mn)
      mn.obj.setSymbol(mSym).setPos(mn.obj)

      return mSym
    }

    def clsSym(cls: ClassDecl): ClassSymbol = {
      var cSym = new ClassSymbol(cls.id.value)

      // Parent
      if (cls.parent.isDefined) {
        var pSym = new ClassSymbol(cls.parent.get.value)
        cSym.parent = Some(pSym)
        cls.parent.setSymbol(pSym).setPos(cls.parent)
      }

      // members
      var classVarMap = Map[String, VariableSymbol]()
      cls.vars.foreach(v => {
        classVarMap += (v.id.value -> varSym(v))
        // Set map of VariableSymbols for members in ClassSymbol
      })
      cSym.members = classVarMap.toMap

      // methods
      var classMthMap = Map[String, MethodSymbol]()
      cls.methods.foreach(m => {
        classMthMap += (m.id.value -> mthSym(m))
      })
      cSym.methods = classMthMap.toMap

      c.setSymbol(cls).setPos(c)
      c.id.setSymbol(cls).setPos(c.id)

      return cSym
    }

    def mthSym(mth: MethodDecl): MethodSymbol = {
      var mthSym = new MethodSymbol(m.id.value)

      // Params
      var paramMap = Map[String, VariableSymbol]()
      mth.args.foreach(p => {
        paramMap += (p.id.value -> formalSymbol(p))
        // Set map of VariableSymbols for params in MethodSymbol
        mth.params = paramMap.toMap
      })

      return mthSym
    } */

    var idClassMap = Map[Identifier, ClassDecl]()
    var classList = ListBuffer[ClassDecl]()

    var classMap = Map[String, ClassSymbol]()

    def progSymbol(p: Program): Program = {
      debug("Enter progSymbol")

      // Set ClassSymbol for mainClass in GlobalScope
      gs.mainClass = mSym


      // Set symbols of classes in program
      
      // Checklista över vilka klasser vi gjort

      p.classes.foreach(c => {
        idClassMap += (c.id -> c)
        })
      /*
      classList = p.classes.to[ListBuffer]
      classList.foreach(c => {
        debug("Processing class: " + c.id.value)
        
        classMap += (c.id.value -> classSymbol(c))
        // Set map of ClassSymbols for classes in GlobalScope
        gs.classes = classMap.toMap
      })
      */
      // skapa klassymboler med bara namn
      classList = p.classes.to[ListBuffer]
      classList.foreach(c => {
        debug("Processing class: " + c.id.value)
        if (!gs.lookupClass(c.id.value).isDefined) {
          var clsym = new ClassSymbol(c.id.value)
          classMap += (c.id.value -> clsym)
        }
        else {
          error("Class defined more than once",c)
        }
        
        // Set map of ClassSymbols for classes in GlobalScope
        gs.classes = classMap.toMap
      })

      // skriv över platsen i mappen med hela innehållet i klassen
      classList.foreach(c => {
          debug("Processing class: " + c.id.value)
          classSymbol(c, classMap)
          //gs.classes = classMap.toMap
        })

      

      // Set symbols of main in program
      mainSymbol(p.main)

      debug("return from progSymbol")
      return p
    }

    def mainSymbol(mn: MainDecl) {
      debug("Enter MainSymbol")

      //Define ClassSymbol for main (görs globalt)
      //var maindecl = new ClassSymbol(mn.obj.value)

      // Parent in main should not have symbol
  
      if (mn.parent == null || mn.parent.value != "App") {
        error("Main object does not extend App type", mn.parent)
      } else {
        var pSym = new ClassSymbol("App")
        mSym.setType(new TAnyRef(pSym))
        mSym.parent = Some(pSym)
      }

      

      // Set symbols of variables in main
      var varMap = Map[String, VariableSymbol]()
      mn.vars.foreach(v => {
        //Check that variable is not defined more than once
        if (mSym.lookupVar(v.id.value).isDefined) {
          error("Variable is defined more than once in main", v)
        }
        varMap += (v.id.value -> varSymbol(v, mSym, None))
        // Set map of VariableSymbols for members in ClassSymbol
        mSym.members = varMap.toMap
      })      

      // Set symbols of expressions in main
      mn.exprs.foreach(e => {
        exprSymbol(e, mSym, None)
      })

      debug("Setting main symbol")

      // Set symbol of MainDecl and its name
      mn.setSymbol(mSym).setPos(mn)
      mn.obj.setSymbol(mSym).setPos(mn.obj)
      debug(mn.obj.value + " " + mn.obj.getSymbol.id + " " + mSym.getType)
      debug("return from mainSymbol")

    }

    def checkForLoops(startClass: Identifier, currentClass: ClassDecl): Boolean = {
      debug("check for loop")
      if (currentClass.parent != None) {
        // Basecase: parent is same as startclass, cycle detected
        if (currentClass.parent.get == startClass) {
          return true
        }
        else {
          checkForLoops(startClass, idClassMap(currentClass.parent.get))
        }
      }
      else {
        // Class does not have parent, cycle does not exist
        return false
      }
    }

    def classSymbol(c: ClassDecl, cm: Map[String, ClassSymbol]) {
      debug("Enter classSymbol")

      var cls = cm(c.id.value)

      // Check that class name is not the same as main class name
      if (c.id.value == gs.mainClass.name) {
        error("Class uses same name as main object", c)
      } /*else if (gs.lookupClass(c.id.value).isDefined) {
          error("Class is defined more than once", c)
      }*/

      // Define ClassSymbol for a class
      //var cls = new ClassSymbol(c.id.value)

      // Set ClassSymbol for parent in ClassSymbol if it exists
      if (c.parent != None) {
        debug("found parent")
        debug("parent is: " + c.parent.get.value)
        if (!idClassMap.contains(c.parent.get)) {
          error("Parent class does not exist", c.parent.get)
        } else {
          // Check for parent loops
          debug("check for parent loops")
          var parentLoop = checkForLoops(c.id, idClassMap(c.id))
          debug("parentLoop = " + parentLoop)
          if (parentLoop) {
            error("The inheritance graph has a cycle", c)
          }
          else {
            // Check if parent is already done
            debug("" + classList)
            if (classList.contains(idClassMap(c.parent.get))) {
              debug("parent: " + c.parent.get.value + " is not done")
              classSymbol(idClassMap(c.parent.get), cm)
              var parentSym = gs.lookupClass(c.parent.get.value)
              cls.parent = parentSym
              debug("set parentsym for class: " + c.id.value + " to class " + c.parent.get)
              c.parent.get.setSymbol(parentSym.get).setPos(c.parent.get)
            }
            else {
              cls.parent = gs.lookupClass(c.parent.get.value)
              c.parent.get.setSymbol(cls.parent.get).setPos(c.parent.get)
            }
            
          }
        }
        /*
        // Set symbol of MainDecl and its name
        c.setSymbol(cls).setPos(c)
        c.id.setSymbol(cls).setPos(c.id)
        debug(c.id.value + " " + c.id.getSymbol.id)
        debug("return from classSymbol") */
      }

      // Set symbols of variables in a class
      debug("checking class variables")
      var classVarMap = Map[String, VariableSymbol]()
      c.vars.foreach(v => {
        // Check that a variable is not defined more than once
        debug("current var: " + v.id.value)
        debug("var exists? " + cls.lookupVar(v.id.value))
        if (cls.lookupVar(v.id.value).isDefined) {
          // class lookupvar kollar både i klassen samt i parentklassen/-erna
          error("Field is defined more than once in class or overloaded", v)
        }
        // nedanstående kommer aldrig bli true för vi har inte gett metoderna symboler ännu?
        
        classVarMap += (v.id.value -> varSymbol(v, cls, None))
        // Set map of VariableSymbols for members in ClassSymbol
        cls.members = classVarMap.toMap
        //classList
      })



      // Set symbols of methods in a class
      debug("checking class methods")
      var classMthMap = Map[String, MethodSymbol]()
      c.methods.foreach(m => {
        // class lookupMethod kollar i både i klassen och parentklassen
        // samma namn är endast okej om det är en  method 
        if (cls.lookupMethod(m.id.value).isDefined && !m.overrides) {
          error("Method is overloading", m.id)
        }
        
        classMthMap += (m.id.value -> methodSymbol(m, cls))       
        // Set map of MethodSymbols for methods in ClassSymbol
        cls.methods = classMthMap.toMap
      })

      

      debug("setting classSymbol")

      // Set symbol of ClassDecl and its name
      c.setSymbol(cls).setPos(c)
      c.id.setSymbol(cls).setPos(c.id)
      /*if(c.parent.isDefined){
        //println("asd")
        cls.setType(convertTypeTreeToType(c,c.parent.get))
      }else{ */
        //println("qwe")
        cls.setType(convertTypeTreeToType(c,c.id))
     // }
      debug(c.id.value + " " + c.id.getSymbol.id + " " + cls.getType)
      debug("return from classSymbol")

      classList -= c
      //return cls
    }

    def methodSymbol(m: MethodDecl, cl: ClassSymbol): MethodSymbol = {
      debug("Enter methodSymbol")

      // Define MethodSymbol for a method
      var mth = new MethodSymbol(m.id.value, cl)

      // Set symbols of params in a method and add them to arglist
      var paramMap = Map[String, VariableSymbol]()
      var args = ListBuffer[VariableSymbol]()
      m.args.foreach(p => {
        // Check that no arguments have the same name
        if (mth.lookupVar(p.id.value) != None) {
          if (p.id.value == mth.lookupVar(p.id.value).get.name) {
            error("Two method arguments have the same name", p)
          }
        }
        var frmSym = formalSymbol(p)
        paramMap += (p.id.value -> frmSym)

        //Add to arglist
        args += frmSym

        // Set map of VariableSymbols for params in MethodSymbol
        mth.params = paramMap.toMap
      })
      mth.argList = args.toList

      // Set symbols of variables in a method
      var mthVarMap = Map[String, VariableSymbol]()
      m.vars.foreach(mem => {
        // Check that a variable is not defined more than once
        // Also checks that the variable does not shadow argument
        if (mth.lookupVar(mem.id.value).isDefined) {
          debug("variable is: " + mth.lookupVar(mem.id.value).get.name)
          error("Variable is defined more than once in method or shadows argument", mem)
        }
        mthVarMap += (mem.id.value -> varSymbol(mem, cl, Some(mth)))
        // Set map of VariableSymbols for members in MethodSymbol
        mth.members = mthVarMap.toMap
      })

      // Check if potentially overridden method exists
      if (m.overrides) {
        // Finns parentklass
        debug("mth belongs to class: " + cl.name)
        //debug("class parent is: " + cl.parent.get.name)
        if (cl.parent.isDefined) {
          if (!cl.parent.get.lookupMethod(m.id.value).isDefined) {
            error("overridden method does not exist", m)
          }
          // Check that # of args in overriden method is correct 
          else if (m.args.size != cl.parent.get.lookupMethod(m.id.value).get.params.size) {
            error("number of args does not match overridden method", m)
          }
          else if (convertTypeTreeToType(m, m.retType) != cl.parent.get.lookupMethod(m.id.value).get.getType) {
            error("Return type does not match overridden method", m.retType)
          }
          // Check that argument types are the same
          else {
            mth.argList.zipWithIndex.foreach{
                case(arg, i) => {
                    debug("oarg: " + arg.getType + " arg: " + cl.parent.get.lookupMethod(m.id.value).get.argList(i).getType)
                    if(arg.getType != cl.parent.get.lookupMethod(m.id.value).get.argList(i).getType){
                        error("Argument types do not match overridden method", m.args(i))                  
                    }
                }
            }
          }
          mth.overridden = cl.parent.get.lookupMethod(m.id.value)

        }
        else {
          error("parent class does not exist in override check", m)
        }
      }
      
      // Set symbols of expressions in a method

      m.exprs.foreach(e => {
        exprSymbol(e, cl, Some(mth))
      })

      // Set symbol of return expression in a method
      exprSymbol(m.retExpr, cl, Some(mth))

      // Set symbol of MethodDecl and its name
      m.setSymbol(mth).setPos(m)
      m.id.setSymbol(mth).setPos(m.id)
      mth.setType(convertTypeTreeToType(m,m.retType))
      debug("setting methodSymbol")
      debug(m.id.value + " " + m.id.getSymbol.id + " " + mth.getType)
      debug("return from methodSymbol")
      return mth
    }

    def varSymbol(vr: VarDecl, cl: ClassSymbol, mth: Option[MethodSymbol]): VariableSymbol = {
      debug("Enter varSymbol")
      debug("evaluating variable: " + vr.id.value)
      // Define VariableSymbol of a variable
      var vrSym = new VariableSymbol(vr.id.value)

      //Check that the expression is only constant, null or New()
      vr.expr match {
        case IntLit(_) | StringLit(_) | True() | False() | Null() | New(_) => {}
        case _ => {
          error("Initializer expression in a variable declaration must be either a constant, null or a New()", vr.expr)
        }
      }

      // Set symbol of the expression in the variable
      exprSymbol(vr.expr, cl, mth)

      // Check that variable is not defined in parent class
      /*if (cl.parent.isDefined) {
        if(cl.parent.get.lookupVar(vr.id.value).isDefined) {
          error("a class member is overriden without keyword override", vr)
        }
      } */


      // Set symbol of VarDecl and its name
      vr.setSymbol(vrSym).setPos(vr)
      vr.id.setSymbol(vrSym).setPos(vr.id)
      vrSym.setType(convertTypeTreeToType(vr,vr.tpe))
      debug("setting variablesymbol")
      debug(vr.id.value + " " + vr.id.getSymbol.id)
      debug("return from varSymbol")
      return vrSym
    }

    def formalSymbol(frm: Formal): VariableSymbol = {
      debug("Enter formalSymbol")

      // Define VariableSymbol of a formal
      var frmSym = new VariableSymbol(frm.id.value)

      debug("setting variablesymbol")

      // Set symbol of Formal and its name
      frm.setSymbol(frmSym).setPos(frm)
      frm.id.setSymbol(frmSym).setPos(frm.id)
      frmSym.setType(convertTypeTreeToType(frm,frm.tpe))
      debug(frm.id.value + " " + frm.id.getSymbol.id + " " + frmSym.getType)
      debug("return from formal")
      return frmSym
    }

    def exprSymbol(exp: ExprTree, cls: ClassSymbol, mth: Option[MethodSymbol]) {

      // Match the correct expression and set symbol
      debug("enter expr")
      debug("expression is " + exp)
      debug("expression belongs to class: " + cls.name)
      exp match {
        case And(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Or(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Plus(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Minus(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Times(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Div(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case LessThan(lhs, rhs) => {
          exprSymbol(lhs, cls, mth)
          exprSymbol(rhs, cls, mth)
        }

        case Equals(lhs, rhs) => {
          debug("run equals")
          exprSymbol(lhs, cls, mth)
          debug("done with left hand side")
          exprSymbol(rhs, cls, mth)
          debug("done with right hand side")
        }
        case While(cond, body) => {
          exprSymbol(cond, cls, mth)
          exprSymbol(body, cls, mth)
        }
        case MethodCall(obj, meth, args) => {
          debug("setting VariableSymbol for MethodCall")
          meth.setSymbol(new VariableSymbol("??")).setPos(meth)
          debug(meth.value + " " + meth.getSymbol.id)
          exprSymbol(obj, cls, mth)
          args.foreach(a => {
            exprSymbol(a, cls, mth)
          })
        }
        case x: Identifier => {
          debug("setting VariableSymbol for Identifier")
          // Pick up symbol for identifier from where it was defined and set it in the expression
          if (mth.isDefined && mth.get.lookupVar(x.value).isDefined) {
            debug("found variable defined in method")
            x.setSymbol(mth.get.lookupVar(x.value).get).setPos(x)
          }
          else if (cls.lookupVar(x.value).isDefined) {
            debug("found variable defined in class")
            x.setSymbol(cls.lookupVar(x.value).get).setPos(x)
          }
          
          else {
            error("An identifier is used as a variable but is not declared", x)
          }
          
          //debug(x.value + " " + x.getSymbol.id)
        }

        case x: This => {
          debug("setting ClassSymbol for This")
          x.setSymbol(cls).setPos(x)
          debug(x + " " + x.getSymbol.id)
        }
        case Not(expr) => {
          exprSymbol(expr, cls, mth)
        }

        case Block(exprs) => {
          exprs.foreach(e => {
            exprSymbol(e, cls, mth)
          })
        }

        case If(expr, thn, els) => {
          exprSymbol(expr, cls, mth)
          exprSymbol(thn, cls, mth)
          if (els.isDefined) {
            exprSymbol(els.get, cls, mth)
          }
        }

        case Println(expr) => {
          debug("found println")
          exprSymbol(expr, cls, mth)
        }

        case New(id) => {
          debug("setting VariableSymbol for New")
          if (gs.lookupClass(id.value).isDefined) {
            id.setSymbol(gs.lookupClass(id.value).get).setPos(id)

          }
          else {
            error("Class name is used as a symbol but is not declared", id)
          }
        }
        case Assign(id, expr) => {
          debug("setting VariableSymbol for Assign")
          if (mth.isDefined && mth.get.params.contains(id.value)) {
            error("Cannot reassign method parameter", id)
          }

          else if (mth.isDefined && mth.get.lookupVar(id.value).isDefined) {
            id.setSymbol(mth.get.lookupVar(id.value).get).setPos(expr)
          }
          else if (cls.lookupVar(id.value).isDefined) {
            id.setSymbol(cls.lookupVar(id.value).get).setPos(expr)
          }
          else {
            error("An identifier is used as a variable but is not declared", id)
          }
          
          //debug(id.value + " " + id.getSymbol.id)
          exprSymbol(expr, cls, mth)
        }
        // Constants (int, string, bool, null does not have symbol)
        case _ => {}

      }
    }
    
    def convertTypeTreeToType(pos :Positioned, tpe: TypeTree): Type = tpe match {
      case _: IntType => TInt
      case _: BooleanType => TBoolean
      case _: StringType => TString
      case _: UnitType => TUnit
      case id: Identifier => gs.lookupClass(id.value) match {
        case Some(clssym) => TAnyRef(clssym)
        case _ => error("No such class " + id.value, pos); TUntyped
      }
    }

    // (Step 3:) Print tree with symbol ids for debugging
    // Make sure you check all constraints
    
    var symres = progSymbol(prog)
    //debug("" + symres.main.getSymbol.id)
    /*symres.classes.foreach(c => {
      debug(c.id.getSymbol.id+"")
      })*/

    debug("end of nameanalysis run")
    terminateIfErrors
    return symres
  }

}

