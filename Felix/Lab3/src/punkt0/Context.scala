package punkt0

import java.io.File

case class Context(
  val files: Option[File] = None,
  val outDir: Option[File] = None,
  val doEval: Boolean = false,
  val doHelp: Boolean = false,
  val doPrint: Boolean = false,
  val doTokens: Boolean = false,
  val doAST: Boolean = false,
  val doSymbolIds: Boolean = false
)
