package punkt0

import java.io.File

case class Context(
  val file: File = null,
  val outDir: Option[File] = None,
  val doEval: Boolean = false,
  val doHelp: Boolean = false,
  val doPrint: Boolean = false,
  val doTokens: Boolean = false,
  val doAST: Boolean = false,
  val doSymbolIds: Boolean = false,
  val doTest: Boolean = false
)
