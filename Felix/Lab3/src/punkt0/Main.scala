package punkt0

import java.io.File
import lexer._
import ast._

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
        ctx = ctx.copy(files = Some(new File(f)))
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
    val d = new File("/home/felix/Documents/Komp17/Felix/Lab3/testprograms/lab3/valid")
    var testExtention = List("p0")
    var checkExtention = List("p0.ast")
    if (d.exists && d.isDirectory) {
      testFiles = d.listFiles.filter(_.isFile).toList.filter{ 
        file => testExtention.exists(file.getName.endsWith(_))
      }
    }
    
    var pass = true;
    var parser : Trees.Program = null
    for(file <- testFiles){
      try{
        val iter = Lexer.run(file)(ctx)
        parser = Parser.run(iter)(ctx)
      }catch{
        case _ : Throwable => pass = false; println("Syntax Error in file: " + file); return;
      }
      var test = parser.toString().toCharArray().iterator
      var check = scala.io.Source.fromFile(file.toString + ".ast").toIterator
      var ok = false
     
      while(test.hasNext && check.hasNext){
        if(test.next().equals(check.next())){
          ok = true
        }else{
          ok = false
        }
      }
      if(ok){
        println("AST PASS: " + file)
      }else{
        println("AST missmatch: " + file)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var input = Array("--test", "/home/felix/Documents/Komp17/Felix/Lab3/testprograms/lab3/valid/Hej.p0");
    val ctx = processOptions(input)
    val runTests = false;
    
    if (ctx.doTokens) {
      val iter = Lexer.run(ctx.files.head)(ctx)
      while (iter.hasNext) {
        val tok = iter.next()
        println(tok+"("+tok.line + ":" + tok.column +")")
      }
    }  else if (ctx.doAST){
      val iter = Lexer.run(ctx.files.head)(ctx)
      val parser = Parser.run(iter)(ctx)
      println(parser)
    } else if (ctx.doPrint){
      val iter = Lexer.run(ctx.files.head)(ctx)
      val parser = Parser.run(iter)(ctx)
      println(Printer.apply(parser))
    } else if (ctx.doTest){
      runAllParserTests(ctx);
    }
    
  }
}
