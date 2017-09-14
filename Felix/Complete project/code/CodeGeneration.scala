package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer



// TODO: gå igenom alla testfall: valid / invalid (lab 5)
// TODO: jämför reporterfil med original

object CodeGeneration extends Phase[Program, Unit] {
  var currClass = ""
  var currMt = new MethodSymbol("", new ClassSymbol(""))
  //var mtStartLabel = 
  var varIdx : HashMap[Int, Int] = new HashMap[Int, Int]()
  //var argsIdx : HashMap[String, Int] = new HashMap[String, Int]()
  var tailOn = false;
  var bugger : Boolean = false
  def debug(str: String) = {
  	if(bugger) {
  		println(str)
  	}
  }

  def run(prog: Program)(ctx: Context): Unit = {


  	// Converts the type of a variable or method to the correct value for class files.
  	def typeTreeToClassType(tpe: TypeTree): String = {
  		tpe match {
  			case BooleanType() => return "Z"
  			case IntType() => return "I"
  			case StringType() => return "Ljava/lang/String;"
  			case UnitType() => return "V"
  			case Identifier(value) => {
  				var res = "L" + value + ";"
  				return res
  			}
      	}
  	}

  	def typeToClassType(tpe: Type): String = {
  		tpe match {
  			case TBoolean => return "Z"
	  		case TInt => return "I"
	  		case TString => return "Ljava/lang/String;"
	  		case TUnit => return "V"
	  		case TAnyRef(x) => {
				var res =  "L" + x.name +";"
				return res
			}
			case _ => sys.error("found TUntyped or TError")
  		}
  		
  	}

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      	// TODO: Create code handler, save to files ...

      	// Creating the class file
      	debug("creating classfile for class: " + ct.id.value)
		val classFile = new ClassFile(ct.id.value, if (ct.parent.isDefined) Some(ct.parent.get.value) else None)

      // Set source file
      debug("set sourcefile to: " + sourceName + " and add def. const.")
      classFile.setSourceFile(sourceName)
      //classFile.addDefaultConstructor
      
      // Set fields
      debug("add fields for class")
      ct.vars foreach {
      	vt => {
      		debug("adding field: " + vt.id.value)
            debug("with type: " + typeTreeToClassType(vt.tpe))
      		val fh : FieldHandler = classFile.addField(typeTreeToClassType(vt.tpe), vt.id.value)
      	}
      }
      debug("done with fields")
      val classCons = classFile.addConstructor(Nil)

      val classCh = classCons.codeHandler
      //parent value eller java/Lang/Object
      //"<init>"
      //()V
     var className = "java/lang/Object"
      if(ct.parent.isDefined) {
      	className = ct.parent.get.value
      } 
      debug("className: " + className)
      //ct.parent.map(_.value).elseOrGet("java/Lang/Object")
      classCh << ALOAD_0 << InvokeSpecial(className, "<init>", "()V")


      //för varje var:
      ct.vars foreach {
      	vt => {
      		classCh << ALOAD_0
      		compileExpr(vt.expr, classCh)

      		classCh << PutField(ct.id.value, vt.id.value, typeTreeToClassType(vt.tpe))
      	}
      }
      classCh << RETURN
      classCh.freeze
      //ch << ALOAD_0
      //compileExpr(v.expr, cch) 
      //ch << putfield

      

      // Set methods
      debug("add methods for class")
      ct.methods foreach {
      	mt => {
      		debug("adding method: " + mt.id.value)
      		var retType = typeTreeToClassType(mt.retType)
      		debug("retType is: " + retType)
      		var params = ""
      		mt.args foreach {
      			mp => {
      				params += typeTreeToClassType(mp.tpe)
      			}
      		}
      		debug("params is: " + params)
      		currClass = ct.id.value
      		debug("currClass is: " + currClass)
      		debug("creating codehandler")
      		val ch: CodeHandler = classFile.addMethod(retType, mt.id.value, params).codeHandler
      		generateMethodCode(ch,mt)
      		debug("done with: " + mt.id.value)
      	}
      }
      debug(dir);
      debug("write to file")
      classFile.writeToFile(dir + ct.id.value + ".class")

      
 
      //???
    }

    def generateMainFile(sourceName: String, mt: MainDecl, dir: String): Unit = {
    	debug("creating classfile for main")
    	val mf = new ClassFile(prog.main.obj.value, None)
    	debug("setting sourcefile of main to: " + sourceName)
    	mf.setSourceFile(sourceName)
    	debug("add defaultconstructor")
    	mf.addDefaultConstructor


    	debug("adds mainmeth and creates codehandler")
    	val ch = mf.addMainMethod.codeHandler
   	 	generateMainMethodCode(ch, prog.main)

    	mf.writeToFile(dir + prog.main.obj.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      currMt = methSym

      // TODO: Emit code

      // Give local vars slots
      debug("clearing varidx")
      varIdx.clear()
      debug("go through method variables")
      mt.vars foreach {
      	mv => {
      		debug("adds: " + mv.id.value + " with id " + mv.getSymbol.id + " to varIdx")
      		varIdx += (mv.getSymbol.id -> ch.getFreshVar)
      		debug("varidx now is: " + varIdx)
      		compileExpr(mv.expr, ch)
    		mv.id.getType match {
    			case TInt => ch << IStore(varIdx(mv.getSymbol.id))
				case TBoolean => ch << IStore(varIdx(mv.getSymbol.id))
				case TString | TAnyRef(_) | TNull => ch << AStore(varIdx(mv.getSymbol.id))
                case _ => sys.error("Expr type in variable declaration went wrong.")
			}
      	}
      }

      //create a label that shows where the method starts (used for tail call opt)
      var mtStartLabel = ch.getFreshLabel("mtStart")
      ch << Label(mtStartLabel)

      // give params slots
      debug("go through params and give them slots")
      //argsIdx.clear()
      mt.args foreach {
      	mp => {
      		debug("adding arg: " + mp.id.value)
      		debug("with index: " + methSym.argList.indexOf(mp.getSymbol))
      		//debug("argsidx is: "+ argsIdx)
      		//argsIdx += (mp.id.value -> methSym.argList.indexOf(mp.getSymbol))
      		var idx = methSym.argList.indexOf(mp.getSymbol) + 1
      		varIdx += (mp.getSymbol.id -> idx)
      		//debug("argsidx is: " + argsIdx)
      		/*compileExpr(mp.expr, ch)
      		mp.id.getType match {
    			case TInt => ch << IStore(argsIdx(mp.id.value))
				case TBoolean => ch << IStore(argsIdx(mp.id.value))
				case _ => ch << AStore(argsIdx(mp.id.value))
			}*/
      	}
      }

      debug("go through method expressions " + mt.exprs)
      mt.exprs foreach {
      	expr => compileExpr(expr, ch)
      	debug("exprtype is: " + expr.getType)
      	if (expr.getType != TUnit) {
      		ch << POP
      	}
      }

      debug("go through return expr " + mt.retExpr)
      compileExpr(mt.retExpr, ch, tailOn, mtStartLabel)


      mt.retType match {
      	case IntType() => ch << IRETURN
      	case BooleanType() => ch << IRETURN
      	case UnitType() => ch << RETURN
      	case _ => ch << ARETURN
      }
      println("method is: " + mt.id.value)
      ch.print
      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, md: MainDecl): Unit = {
    	debug("generates mainmeth code")
    	debug("clears varidx and argsidx")
    	varIdx.clear() // Won't be needed anymore
    	//argsIdx.clear() // Won't be needed anymore

    	debug("go through main variable")
    	md.vars foreach {
    		mv => {
    			debug("adds: " + mv.id.value + " with id " + mv.getSymbol.id + " to varidx")
    			varIdx += (mv.getSymbol.id -> ch.getFreshVar)
    			compileExpr(mv.expr, ch)
    			mv.id.getType match {
    				case TInt => ch << IStore(varIdx(mv.getSymbol.id))
					case TBoolean => ch << IStore(varIdx(mv.getSymbol.id))
					case TString | TAnyRef(_) | TNull => ch << AStore(varIdx(mv.getSymbol.id))
                    case _ => sys.error("Expr type in main method var declaration went wrong")
    			}
    		}
    	}

    	debug("go through main exprs")
    	md.exprs foreach {
    		expr => compileExpr(expr, ch)
    		if (expr.getType != TUnit) {
      			ch << POP
      		}
    	}

    	ch << RETURN
    	//ch.print
    	ch.freeze

    }


    def compileExpr(expr: ExprTree, ch: CodeHandler, checkTail: Boolean = false, mtStrtLbl: String = ""): Unit = {
    	debug("-----------compiling expr: " + expr + "------------")
    	//ch << LineNumber(expr.line)
    	expr match {
    		// Tested
    		case And(lhs, rhs) => {
    			compileExpr(lhs, ch)
    			compileExpr(rhs, ch)
    			ch << IAND
    		}
    		// Tested ( with lazy evaluation)
			case Or(lhs, rhs) => {
				val lazyLabel = ch.getFreshLabel("lazy")
				val doneLabel = ch.getFreshLabel("done")
				compileExpr(lhs, ch)
				ch <<
				IfNe(lazyLabel)
				compileExpr(rhs, ch)
				ch <<
				Goto(doneLabel) <<
				Label(lazyLabel) <<
				ICONST_1 <<
				Label(doneLabel)				
			}
			// Tested 
			case Plus(lhs, rhs) => {
				lhs.getType match {
					case TString => {
						rhs.getType match {
							case TString => {
								ch << DefaultNew("java/lang/StringBuilder")
								compileExpr(lhs, ch)
								ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                        		compileExpr(rhs, ch)
                        		ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                        		InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
							}
							case TInt => {
								ch << DefaultNew("java/lang/StringBuilder")
								compileExpr(lhs, ch)
								ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                        		compileExpr(rhs, ch)
                        		ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(I)Ljava/lang/StringBuilder;") <<
                        		InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
							}
							case _ => sys.error("Wrong type for plus expression in code generation")
						}
					}
					case TInt => {
						rhs.getType match {
							case TString => {
								ch << DefaultNew("java/lang/StringBuilder")
								compileExpr(lhs, ch)
								ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(I)Ljava/lang/StringBuilder;")
                        		compileExpr(rhs, ch)
                        		ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        		"(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                        		InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

							}
							case TInt => {
								compileExpr(lhs, ch)
								compileExpr(rhs, ch)
								ch << IADD
							}
							case _ => sys.error("Wrong type for plus expression in code generation")
						}
					}
					case _ => sys.error("Wrong type for plus expression in code generation")
				}
			}
			// Tested (large numbers, +,-, flera termer) TODO: jmfr prio med * /
			// Test larger numbers: växlar mellan ICONST (0-5),BIPUSH, SIPUSH, LDC
			case Minus(lhs, rhs) => {
				compileExpr(lhs, ch)
				compileExpr(rhs, ch) 
				ch << ISUB
			}
			// Tested, även prio
			case Times(lhs, rhs) => {
				compileExpr(lhs, ch)
				compileExpr(rhs, ch)
				ch << IMUL
			}
			// Tested, även prio
			case Div(lhs, rhs) => {
				compileExpr(lhs, ch)
				compileExpr(rhs, ch) 
				ch << IDIV
			}
			// Tested, även prio
			case LessThan(lhs, rhs) => {
				val trueLabel = ch.getFreshLabel("true")
				val doneLabel = ch.getFreshLabel("done")
				compileExpr(lhs, ch)
				compileExpr(rhs, ch)
				ch <<
				If_ICmpLt(trueLabel) <<
				ICONST_0 <<
				Goto(doneLabel) <<
				Label(trueLabel) <<
				ICONST_1 <<
				Label(doneLabel)
			}
			// Tested, även prio
			case Equals(lhs, rhs) => {
				val trueLabel = ch.getFreshLabel("true")
				val doneLabel = ch.getFreshLabel("done")
				compileExpr(lhs, ch)
				compileExpr(rhs, ch)
				lhs.getType match {
					case TInt | TBoolean => {
						ch <<
						If_ICmpEq(trueLabel)
					}
					case TString | TAnyRef(_) | TNull => {
						ch << 
						If_ACmpEq(trueLabel)
					}
					case _ => {
						sys.error("Equals went wrong")
					}
				}
				ch <<
				ICONST_0 <<
				Goto(doneLabel) <<
				Label(trueLabel) <<
				ICONST_1 <<
				Label(doneLabel)
			}
			// Not tested
			case MethodCall(obj, meth, args) => {
				// The function we want to call
				val methSym = meth.getSymbol.asInstanceOf[MethodSymbol]
				//The function from which we want to run
				val runningMeth = currMt

				// If tailcallopt and the methods are the same (it is calling itself), we have tailrec.
				if (methSym == runningMeth && checkTail) {
					var argLength = args.size
					args foreach {
						// Compile the args as usual and store them as local variables
						arg => {
							compileExpr(arg, ch)
						}
						/*arg.getType match {
								case TInt | TBoolean => ch << IStore(argLength)
								case _ => ch << AStore(argLength)
						}
							argLength += 1  */
						
					}
					args foreach {
						arg => {
							arg.getType match {
								case TInt | TBoolean => ch << IStore(argLength)
								case _ => ch << AStore(argLength)
							}
							argLength -= 1
						}
						
					}
					// go to the beginning of the method
					ch << Goto(mtStrtLbl)

				} else {
					//debug("found methodcall")
					//debug("compile obj (" + obj + ")")
					compileExpr(obj, ch)
					//debug("done compiling obj")
					//debug("compiling args")
					//var i = 0
					args foreach {
						debug("compiling args: " + args)
						arg => {
							/*compileExpr(arg, ch)
							debug("Storing var in: " + methSym.argList(i).name )
							debug("argsidx is: " + argsIdx)
							methSym.argList(i).getType match {
								case TInt => ch << IStore(argsIdx(methSym.argList(i).name))
								case TBoolean => ch << IStore(argsIdx(methSym.argList(i).name))
								case _ => ch << AStore(argsIdx(methSym.argList(i).name))
							}
							i = i +1 */
							compileExpr(arg, ch)
						}
					}
					debug("done with args")
					
					val clsName = methSym.classSymbol.name
					debug("extracting class name from methsym (" + clsName +")")
					val mthName = methSym.name
					debug("method name is: " + mthName)
					var types = "(" + methSym.argList.map(a => typeToClassType(a.getType)).mkString("") + ")" + typeToClassType(methSym.getType)
					/*var types = "("
					args foreach {
						arg => {
							debug("arg is: " + arg)
							types += typeToClassType(arg.getType)
						}
					}
					types +=  ")"*/
					//debug("types is now: " + types)
					//types += typeToClassType(methSym.getType)
					//debug("type string is: " + types)
					ch << InvokeVirtual(methSym.classSymbol.name, meth.value, "(" + methSym.argList.map(a => typeToClassType(a.getType)).mkString("") + ")" + typeToClassType(methSym.getType))
					//ch.print
					debug("done with methodcall")
				}
				
			}
			// Tested
			case IntLit(value) => {
				debug("expr was int with value: " + value)
				ch << Ldc(value)
				//ch.print
				debug("done with intlit")

			}
			// Tested
			case StringLit(value) => {
				debug("expr was string with value: " + value)
				ch << Ldc(value)
				//ch.print
				debug("done with stringlit")
			}
			// Tested
			case _: True => {ch << ICONST_1}
			// Tested
			case _: False => {ch << ICONST_0}
			// Tested with local vars in main
			// TODO: test with args, fields and local class methods vars
			case x: Identifier => {
				debug("expr was identifier")
				// Local variable or parameter
				debug("" + varIdx)
				if (varIdx.contains(x.getSymbol.id)) {
					debug("variable is local")
					x.getType match {
						case TInt => ch << ILoad(varIdx(x.getSymbol.id))
						case TBoolean => ch << ILoad(varIdx(x.getSymbol.id))
						case _ => ch << ALoad(varIdx(x.getSymbol.id))
					}
				}
				// Parameter
				/*else if (argsIdx.contains(x.value)) {
					x.getType match {
						case TInt => ch << ILoad(argsIdx(x.value))
						case TBoolean => ch << ILoad(argsIdx(x.value))
						case _ => ch << ALoad(argsIdx(x.value))
					}
				}*/
				// Class field
				else {
					ch << ALoad(0) 
					ch << GetField(currClass, x.value, typeToClassType(x.getType))
				}
			}
			// Not tested (wait for classes)
			case This() => {
				debug("compiling this")
				ch << ALOAD_0
				//ch.print
				debug("done with this")
			}
			// Tested
			case Null() => {ch << ACONST_NULL}
			// Tested (TODO: Testa tillsammans med methodcall)
			case New(tpe) =>  ch << DefaultNew(tpe.value)
			// Tested
			case Not(expr) => {
				val trueLabel = ch.getFreshLabel("true")
				val doneLabel = ch.getFreshLabel("done")
				compileExpr(expr, ch)
				ch <<
				IfEq(trueLabel) <<
				ICONST_0 <<
				Goto(doneLabel) <<
				Label(trueLabel) <<
				ICONST_1 <<
				Label(doneLabel)
			}
			// Tested 
			case Block(exprs) => {
				var blockSize = exprs.size;
				if (blockSize != 0) {
					exprs.init foreach {
						expr => compileExpr(expr, ch)
						if(expr.getType != TUnit) {
							ch << POP
						}
					}
					compileExpr(exprs.last, ch, tailOn, mtStrtLbl)	
				}
			}
			// Tested
	
			case If(expr, thn, els) => {
				val doneLabel = ch.getFreshLabel("done")
				compileExpr(expr, ch)
						
				if (els.isDefined) {
					val elseLabel = ch.getFreshLabel("else")
					// If expr is false then 0 is on stack and ifeq will evaluate to true --> we want to got with else
					ch << 
					IfEq(elseLabel)
					compileExpr(thn, ch, tailOn, mtStrtLbl)
					ch << Goto(doneLabel) <<
					Label(elseLabel)
					compileExpr(els.get, ch, tailOn, mtStrtLbl)
					ch << Label(doneLabel)
				} else {
					ch <<
					// If expr is false then 0 is on stack and ifeq will evaluate to true --> no else exists, we are done
					IfEq(doneLabel)
					compileExpr(thn, ch)
					ch << Label(doneLabel)
				}	
			}
			// Tested
			
			case While(cond, body) => {
				val loopLabel = ch.getFreshLabel("loop")
				val doneLabel = ch.getFreshLabel("done")

				ch << 
				Label(loopLabel)
				compileExpr(cond, ch)
				ch << IfEq(doneLabel)
				compileExpr(body, ch)
				ch << 
				Goto(loopLabel) <<
				Label(doneLabel)
			}
			// Tested (TODO: måste man använda stringbuilder? java tillåter int och bool för println)
			case Println(expr) => {
				debug("expr was println")
				expr.getType match {
					case TString => {
						debug("expr was type string")
						ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
					    compileExpr(expr, ch)
					    ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
					    //ch.print
					}
					case TInt => {
						debug("expr was type int")
						ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
						//ch << DefaultNew("java/lang/StringBuilder")
						compileExpr(expr, ch)
						debug("done with compiling expr")
						//ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        //"(I)Ljava/lang/StringBuilder;") <<
                        //InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
                        ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
                        debug("done with println")
                        //ch.print
					}
					case TBoolean => {
						debug("expr was type boolean")
						ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")  
						//ch << DefaultNew("java/lang/StringBuilder") 
						compileExpr(expr, ch)
						//ch << InvokeVirtual("java/lang/StringBuilder", "append",
                        //"(Z)Ljava/lang/StringBuilder;") <<
                        //InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;") <<
                        //InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
						ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
						//ch.print
					}
					case _ => sys.error("Wrong type for println in code generation.")
				}
			}
			// Tested (TODO: Parameters, class fields)
			case x: Assign => {
				
				// Local variable
				if (varIdx.contains(x.id.getSymbol.id)) {
					compileExpr(x.expr, ch)
					debug("type is: " + x.expr.getType)
					x.expr.getType match {
						case TInt => ch << IStore(varIdx(x.id.getSymbol.id))
						case TBoolean => ch << IStore(varIdx(x.id.getSymbol.id))
						case _ => ch << AStore(varIdx(x.id.getSymbol.id))
					}
				} 
				/*// Parameter
				else if (argsIdx.contains(x.id.value)) {
					Reporter.error("Parameter is not allowed to be reassigned", x)
				} */
				// Class Field
				else {
					ch << ALOAD_0
					compileExpr(x.expr, ch)
                    debug("assigning value to field: " + x.id.value)
                    debug("Type is: " + typeToClassType(x.getType))
					ch << PutField(currClass, x.id.value, typeToClassType(x.id.getType))
				}
			}
    	}
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    debug("-------------------------compile classes------------------------")
    prog.classes foreach {
      	ct => {
      		debug("--------------compiling class: " + ct.id.value + "---------------")
      		generateClassFile(sourceName, ct, outDir)
      	}
      	debug("-------------------done with class---------------------------")
    }

    // Now do the main declaration
    // ...
    debug("-------------------------compile main------------------------")
    generateMainFile(sourceName, prog.main, outDir)
    debug("-------------------------done with main------------------------")
  }

}
