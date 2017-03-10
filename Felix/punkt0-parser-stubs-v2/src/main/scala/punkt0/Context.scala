package punkt0

import java.io.File

case class Context(
  file: File = null,
  outDir: Option[File] = None,
  doEval: Boolean = false,
  doHelp: Boolean = false,
  doPrintMain: Boolean = false,
  doTokens: Boolean = false,
  doAST: Boolean = false,
  doSymbolIds: Boolean = false,
  doPrint: Boolean = false,
  doTest: Boolean = false
)
