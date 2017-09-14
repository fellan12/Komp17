package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._
import Reporter._


object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */

    var bugger = false
    def debug(str: String) = {
      if (bugger) {
        println(str)
      }
    }
    // TODO: variabeldeklaration tillåter just nu att man deklarerar en variabel till en subtyp av typen
    // dvs. class A extends B, var i: B = new A() är tillåtet för att A är subtyp av B.
    // TODO: Ta bort error färg röd

  def run(prog: Program)(ctx: Context): Program = {

    def lub(thn: Type, els: Type): Type = {
      thn match {
        case TUnit | TInt | TString | TBoolean => {
          if (thn == els) {
            return thn
          } else {
            error("primitive types in if else must match exactly")
            TError
          }
        }
        case TAnyRef(thnClsSym) => {
          els match {
            case TUnit | TInt | TString | TBoolean => {
              error("els cannot be primitive when thn is TAnyref")
              TError
            }
            case TNull => {
              return thn
            }
            case TAnyRef(elsClsSym) => {
              if(thn.isSubTypeOf(els)) {
                return els
              }
              else if (els.isSubTypeOf(thn)) {
                return thn
              }
              if (thnClsSym.parent.isDefined) {
                lub(thnClsSym.parent.get.getType, els)
              }
              else {
                return anyRef
              }
            }
            case _ => {
              error("then is TAnyRef but els is untyped eller TError")
              TError
            }
          }
        }
        case TNull => {
          els match {
            case TNull => {
              return TNull
            }
            case TAnyRef(_) => {
              return els
            }
            case _ => {
              error("thn is TNull, els is not TAnyref ")
              TError
            }
          }
        }
        case _ => {
          error("thn is untyped or TError")
          TError
        }
      }
    }


    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      debug("enter tcexpr")
      debug("expr: " + expr)
      debug("expected: " + expected)
      val tpe: Type = expr match {
        
        
        case And(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TBoolean))
          rhs.setType(tcExpr(rhs, TBoolean))
           TBoolean
        }
        
        case Or(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TBoolean))
          rhs.setType(tcExpr(rhs, TBoolean))
          TBoolean
        }
        
        case Plus(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TInt, TString))
          rhs.setType(tcExpr(rhs, TInt, TString))
          debug("expected now is is: " + expected)
          // Return TInt or TString depending on Plus result
          if(lhs.getType == TInt && rhs.getType == TInt){
            debug("both types were int")
             TInt
          }else {
            debug("one type were string")
             TString
          }
        }

        case Minus(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TInt))
          rhs.setType(tcExpr(rhs, TInt))
           TInt
        }

        case Times(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TInt))
          rhs.setType(tcExpr(rhs, TInt))
           TInt
        }

        case Div(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TInt))
          rhs.setType(tcExpr(rhs, TInt))
           TInt
        }

        case LessThan(lhs,rhs) => {
          //Evaluate type for lhs and rhs and set type
          lhs.setType(tcExpr(lhs, TInt))
          rhs.setType(tcExpr(rhs, TInt))
           TBoolean
        }

        case x : Equals => {
          //Evaluate type for lhs and rhs and set type
          x.lhs.setType(tcExpr(x.lhs))
          x.rhs.setType(tcExpr(x.rhs))

          //Type comparison of two objects
          if(x.lhs.getType.isSubTypeOf(anyRef) && x.rhs.getType.isSubTypeOf(anyRef)) { 
             TBoolean 
          }

          //Type comparison of same primitve type
          else if(x.lhs.getType == x.rhs.getType) { 
             TBoolean
          }

          //Invalid comparison
          else { 
            error("Invalid type comparison", x); 
             TError 
          }
        }

        case x: MethodCall =>{
          // check that the object is a declared class
          x.obj.setType(tcExpr(x.obj, anyRef))

          // The method is correctly declared in the class (and set the method symbol)
          x.obj.getType match {
            case TAnyRef(clssym) => {
              clssym.lookupMethod(x.meth.value) match {
                case Some(mtdsym) => {
                  // set method symbol
                  x.meth.setSymbol(mtdsym)

                  // Check the number of args and their types
                  if (x.args.size != mtdsym.argList.size) {
                    error("Method call with wrong number of arguments", x)
                  } else {
                    // Check argument types
                    x.args.zipWithIndex.foreach{
                      case(arg, i) => arg.setType(tcExpr(arg, mtdsym.argList(i).getType))
                    }
                  }

                  mtdsym.getType
                }
                case _ => {
                  error("Method is not declared in class", x)
                   TError
                }
              }
            }
            case _ => {
              error("Class is not declared", x)
               TError
            }
          }
        }

        case x: This => x.getSymbol.getType

        case x: New => x.tpe.getType

        case x: Null => TNull

        case Not(expr) => {
          //Evaluate type for expr and set type
          expr.setType(tcExpr(expr,TBoolean))
           TBoolean
        }

        case Block(exprs)  => {
          //Evaluate expressions in block and set type
          exprs.foreach(e => e.setType(tcExpr(e)))
          if (exprs.nonEmpty) {
            exprs.last.getType  //Return type
          } else {
             TUnit
          }
        }

        case If(expr, thn, els) => {
          //Evaluate if expressions and set type
          expr.setType(tcExpr(expr, TBoolean))

          if (els.isDefined) {
            val thenType = tcExpr(thn)
            debug("thentype is: " + thenType)
            thn.setType(thenType)
            val elsType = tcExpr(els.get)
            debug("elsetype is: " + elsType)
            els.get.setType(elsType)
            //println(lub(thenType, elsType))
            lub(thenType, elsType)

          } else {
            // if there is no else then thn must be unit
           thn.setType(tcExpr(thn, TUnit))
           TUnit
          }
        }

        case While(cond, body) => {
          //Evaluate while expression and set type
          cond.setType(tcExpr(cond, TBoolean))
          body.setType(tcExpr(body, TUnit))
           TUnit
        }

        case Println(expr) => {
          //Evaluate Println expressions adn set type
          expr.setType(tcExpr(expr, TString, TInt, TBoolean))
           TUnit
        }

        case Assign(id, expr) => {
          //Evaluate Assign and set type
          expr.setType(tcExpr(expr, id.getType))
           TUnit
        }
        // x = 3
        // var x : String = 3

        case x: Identifier => x.getType
        //Return type for int, string, true, false
        case _: IntLit => TInt
        case _: StringLit => TString
        case _: True => TBoolean
        case _: False => TBoolean

        //No expressions match
        case _ => TError
      }
      
      debug("Got TPE: " + tpe + " Expreted " + expected)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        debug("expected is empty")
        debug("#####################################################################################################################")

        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        debug("expected type or subtype of: " + expected + " was not correct")
        debug("#####################################################################################################################")

        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        debug("expected is same or supertype of " + tpe)
        debug("#####################################################################################################################")

        tpe
      }

      
    }

    //MAIN METHOD CHECK
      //main method expressions is correctly typed
      debug("evaluating expressions in main")
      prog.main.exprs.foreach(expr => {
        debug("evaluating expression: " + expr)
        expr.setType(tcExpr(expr))
      })
      
      //main method variables is correctly typed
      debug("evaluationg variables in main")
      prog.main.vars.foreach(vr => {
        debug("evaluating variable: " + vr.id.value)
        tcExpr(vr.expr, vr.getSymbol.getType)
      })

    //CLASS CHECK
    debug("evaluating classes")
    prog.classes.foreach(c => {
      debug("evaluating class: " + c.id.value)
      //class variables is correctly typed
      debug("evaluating variables in class: " + c.id.value)
      c.vars.foreach(vr => {
        debug("evaluating variable: " + vr.id.value + " in class " + c.id.value)
        tcExpr(vr.expr, vr.getSymbol.getType)
      })

      debug("evaluating methods in class: " + c.id.value)
      c.methods.foreach(mth => {
        debug("evaluating method: " + mth.id.value + " in class " + c.id.value)
        //class method retType is correctly typed
        debug("evaluating retexpr " + mth.retExpr + " in method " + mth.id.value)
        mth.retExpr.setType(tcExpr(mth.retExpr, mth.getSymbol.getType))

        debug("evaluating variables in method " + mth.id.value)
        //class method variables is correctly typed
        mth.vars.foreach(vr => {
          debug("evaluating variable: " + vr.id.value + " in method " + mth.id.value)
          //debug("expecting")
          tcExpr(vr.expr, vr.getSymbol.getType)
        })

        debug("evaluating expressions in method " + mth.id.value)
        //class method expressions is correctly typed
        mth.exprs.foreach(expr => {
          debug("evaluation expression: " + expr + " in method " + mth.id.value)
          expr.setType(tcExpr(expr))
        })
      })

    })

    terminateIfErrors
    prog
  }

}
