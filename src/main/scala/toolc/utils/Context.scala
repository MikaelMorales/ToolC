package toolc
package utils

import java.io.File

case class Context(
  val reporter: Reporter,
  val files: List[File] = Nil,
  val outDir: Option[File]
)
