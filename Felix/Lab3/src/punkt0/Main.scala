package punkt0

import java.io.File
import lexer._


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
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
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
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    var input = Array("--token", "/home/felix/Documents/Komp17/Felix/Lab2/testprograms/lab2/valid/positions.p0");
    val ctx = processOptions(input)

    if (ctx.doTokens) {
      val iter = Lexer.run(ctx.files.head)(ctx)
      while (iter.hasNext) {
        val n = iter.next()
        println(n+"("+n.line+":"+n.column+")")
      }
    }    
  }

}
