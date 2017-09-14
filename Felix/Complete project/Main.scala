package punkt0

import java.io.File
import java.util.Scanner
import java.io._
import lexer._
import ast._
import analyzer._
import code._
import scala.collection.mutable.ListBuffer


object Main {
  var printPath : String = "" 
  var validPath : String = ""

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()
    
    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)
        
      case "--print" :: args =>
        ctx = ctx.copy(doPrint = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)
      
      case "--test" :: args =>
        ctx = ctx.copy(doTest = true)
        processOption(args)

      case "--symid" :: args => 
        ctx = ctx.copy(doSymbolIds = true)
        processOption(args)

      case "--eval" :: args =>
        ctx = ctx.copy(doEval = true)
        processOption(args)
        
      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)       

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)

    } else if (ctx.doTokens) {

      val iter = Lexer.run(ctx.file.get)(ctx)

      while (iter.hasNext) {
        val tok = iter.next()
        println(tok+"("+tok.line + ":" + tok.column +")")
      }

    } else if (ctx.doAST){

      val iter = Lexer.run(ctx.file.get)(ctx)
      val parser = Parser.run(iter)(ctx)
      println(parser)
      
    } else if (ctx.doPrint){
      val iter = Lexer.run(ctx.file.get)(ctx)
      val parser = Parser.run(iter)(ctx)
      println(Printer.apply(parser))
      
    } else if (ctx.doSymbolIds){
      val iter = Lexer.run(ctx.file.get)(ctx)
      val parser = Parser.run(iter)(ctx)
      //println(parser)
      val symid = NameAnalysis.run(parser)(ctx)
      //println(symid)
      val symbolicPrint = Printer.apply(symid)
      println(symbolicPrint)
      
    } else if (ctx.doEval){
      val iter = Lexer.run(ctx.file.get)(ctx)
      val parser = Parser.run(iter)(ctx)
      val symid = NameAnalysis.run(parser)(ctx)
      val tpeCheck = TypeChecking.run(symid)(ctx)

    } else if (ctx.doTest){
      runAllParserTests(ctx);
      runAllPrinterTests(ctx);
    }
    else {
      val iter = Lexer.run(ctx.file.get)(ctx)
      val parser = Parser.run(iter)(ctx)
      val symid = NameAnalysis.run(parser)(ctx)
      val tpeCheck = TypeChecking.run(symid)(ctx)
      val codeGen = CodeGeneration.run(tpeCheck)(ctx)
      

    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: ./slacc [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      displays the list of tokens")
    println(" --ast         displays the AST")
    println(" --print       pretty-prints the program")
    println(" --symid       displays symbolic ids on a pretty-print output")

    println(" --eval        Apply type checking to program")

    println(" --test        Run Tests {YOU NEED TO SPECIFY CORRECT PATH TO /VALID IN MAIN METHOD}")
    println(" -d <outdir>   generates class files in the specified directory")
  }
  
  def runAllParserTests(ctx : Context) : Unit = {
    var testFiles = List[File]()
    val d = new File(validPath)    
    var testExtention = List("p0")
    var checkExtention = List("p0.ast")
    if (d.exists && d.isDirectory) {
      testFiles = d.listFiles.filter(_.isFile).toList.filter{ 
        file => testExtention.exists(file.getName.endsWith(_))
      }
    }
    
    var pass = true;
    var parser : Trees.Program = null
    var files = ListBuffer.empty ++= testFiles.sorted
    files -= new File(validPath+ "Hej.p0")  
    for(file <- files){
      try{
        val iter = Lexer.run(file)(ctx)
        parser = Parser.run(iter)(ctx)
      }catch{
        case _ : Throwable => {
          pass = false; println("Syntax Error in file: " + file.toString()); 
          System.exit(1)
        }
      }
      var test = parser.toString().toCharArray().iterator
      var check = scala.io.Source.fromFile(file.toString + ".ast").toIterator
      var ok = false
      var testError : String = "";
      var checkError : String = "";
      while(test.hasNext && check.hasNext){
        testError += test.next()
        checkError += check.next()
        if(testError.equals(checkError)){
          ok = true
        }else{
          ok = false
          println("AST missmatch: " + file.toString())
          for(i <- 1 to 20){
            testError += test.next()
            checkError += check.next()
          }
          println("Tested:")
          println(testError)
          println("Correct:")
          println(checkError)
          System.exit(1)
        }
      }
      
      if(ok){
        println("AST PASS: " + file.toString())
      }
    }
  }
  
  def runAllPrinterTests(ctx : Context) : Unit = {
    var testFiles = List[File]()
    val d = new File(validPath)
    var testExtention = List("p0")
    if (d.exists && d.isDirectory) {
      testFiles = d.listFiles.filter(_.isFile).toList.filter{ 
        file => testExtention.exists(file.getName.endsWith(_))
      }
    }
    
    var pass = true;
    var firstParsed : Trees.Program = null
    var secondParsed : Trees.Program = null
    var firstPrinted : String = ""
    var secondPrinted : String = ""
    var files = ListBuffer.empty ++= testFiles.sorted
    files -= new File(validPath + "Hej.p0")
    for(file <- files){
      try{
        //First Parse
        val iter = Lexer.run(file)(ctx)
        firstParsed = Parser.run(iter)(ctx)
        firstPrinted = Printer.apply(firstParsed)
        
        //Print to File
        var printedFile = new File(file.toString() + ".print")
        var writer : BufferedWriter = null;
        if(!printedFile.exists){
          file.createNewFile()
          writer = new BufferedWriter(new FileWriter(printedFile))
          writer.write(firstPrinted)
          writer.close
        }else{
          writer = new BufferedWriter(new FileWriter(printedFile))
          writer.write(firstPrinted)
          writer.close
        }
        
        //Second Parse
        secondParsed = Parser.run(Lexer.run(printedFile)(ctx))(ctx)
        secondPrinted = Printer.apply(firstParsed)

        
       }catch{
          case _ : Throwable => {
          pass = false; println("Printer Error in file: " + file.toString()); 
          System.exit(1)
        }
      }
     
      var test = firstParsed.toString().toCharArray().iterator
      var check = secondParsed.toString().toCharArray().iterator
      var ok = false
      var testError : String = "";
      var checkError : String = "";
      while (test.hasNext && check.hasNext) {
        testError += test.next()
        checkError += check.next()
        if (testError.equals(checkError)) {
          ok = true
        } else {
          ok = false
          println("Print missmatch: " + file.toString())
          for(i <- 1 to 20){
            testError += test.next()
            checkError += check.next()
          }
          println("First:")
          println(testError)
          println("Second:")
          println(checkError)
          System.exit(1)
        }
      }
      
      if (ok) {
        println("Print PASS: " + file.toString())
      }
    }
  }

    
  def main(args: Array[String]): Unit = {
    //var input = Array("--tokens", "/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Lab3/testprograms/lab2/valid/hej.p0");
    validPath = "/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Lab3/testprograms/lab3/valid/"
    var input = Array("--tokens", "./Hej.p0");
    //validPath = "./testprograms/lab3/valid/"
    val ctx = processOptions(args)
    
    Reporter.terminateIfErrors
  }
}