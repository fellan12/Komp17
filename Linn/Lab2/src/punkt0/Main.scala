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
        var lxr = Lexer
        var it = lxr.run(new File(args.last))(ctx)
        while ( it.hasNext ) {
          var tok = it.next;
          println(tok + "(" + tok.line + ":" + tok.column + ")");
        }
        //println(it.next());
        
      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
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
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
    println(" --token				prints list of tokens in file")
  }

  def main(args: Array[String]): Unit = {
    var input = Array("--token", "/home/bergelid/Dropbox/KTH/År 4/kompkons/Komp17/Linn/Lab2/testprograms/lab2/invalid/005.p0");
    val ctx = processOptions(input)

    // TODO: run lexer phase
    
  }

}
