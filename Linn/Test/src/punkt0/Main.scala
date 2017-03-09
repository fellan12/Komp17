package punkt0

import java.io.File
import java.util.Scanner
import java.io._
import lexer._
import ast._
import scala.collection.mutable.ListBuffer


object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--token" :: args =>
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
        
      case f :: args =>
        ctx = ctx.copy(file = new File(f))
        processOption(args)       

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: ./slacc [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      displays the list of tokens")
    println(" --print       pretty-prints the program")
    println(" --ast         displays the AST")
    println(" --test				Run Tests")
    println(" -d <outdir>   generates class files in the specified directory")
  }
  
  def runAllParserTests(ctx : Context) : Unit = {
    var testFiles = List[File]()
    val d = new File("/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Test/testprograms/lab3/valid")
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
    files -= new File("/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Test/testprograms/lab3/valid/Hej.p0")
    for(file <- files){
      try{
        val iter = Lexer.run(file)(ctx)
        parser = Parser.run(iter)(ctx)
      }catch{
        case _ : Throwable => {
          pass = false; println("Syntax Error in file: " + file.toString().substring(57).toString()); 
          return;
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
          println("AST missmatch: " + file.toString().substring(57).toString())
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          println(testError)
          println(checkError)
          return
        }
      }
      
      if(ok){
        println("AST PASS: " + file.toString().substring(57).toString())
        println(testError)
        println(checkError)
      }
    }
  }
  
  def runAllPrinterTests(ctx : Context) : Unit = {
    var testFiles = List[File]()
    val d = new File("/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Test/testprograms/lab3/valid")
    var testExtention = List("p0")
    if (d.exists && d.isDirectory) {
      testFiles = d.listFiles.filter(_.isFile).toList.filter{ 
        file => testExtention.exists(file.getName.endsWith(_))
      }
    }
    
    var pass = true;
    var firstParsed : Trees.Program = null
    var secondParsed : Trees.Program = null    
    var files = ListBuffer.empty ++= testFiles.sorted
    files -= new File("/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Test/testprograms/lab3/valid/Hej.p0")
    for(file <- files){
      try{
        //First Parse
        val iter = Lexer.run(file)(ctx)
        firstParsed = Parser.run(iter)(ctx)
        var firstPrinted = Printer.apply(firstParsed)
        
        //Print to File
        var printedFile = new File("/home/felix/Documents/Komp17/Felix/Lab3/testprograms/lab3/prints/" + file.toString().substring(64).toString() + ".print")
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
        
       }catch{
          case _ : Throwable => {
          pass = false; println("Printer Error in file: " + file.toString().substring(57).toString()); 
          return;
        }
      }
     
      var test = firstParsed.toString().toCharArray().iterator
      var check = secondParsed.toString().toCharArray().iterator
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
          println("Print missmatch: " + file.toString().substring(57).toString())
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          testError += test.next()
          checkError += check.next()
          println(testError)
          println(checkError)
          return
        }
      }
      
      if(ok){
        println("Print PASS: " + file.toString().substring(57).toString())
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var input = Array("--ast", "/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Test/testprograms/lab3/valid/VehicleRent.p0");
    val ctx = processOptions(input)
    val runTests = false;
    val iter = Lexer.run(ctx.file)(ctx)
    val parser = Parser.run(iter)(ctx)

    if (ctx.doTokens) {
      while (iter.hasNext) {
        val tok = iter.next()
        println(tok+"("+tok.line + ":" + tok.column +")")
      }
      
    } else if (ctx.doAST){
      println(parser)
      
      
    } else if (ctx.doPrint){
      println(Printer.apply(parser))
      
    } else if (ctx.doTest){
      runAllParserTests(ctx);
      //runAllPrinterTests(ctx);
    }
    
  }
}
