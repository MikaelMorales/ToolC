package toolc
package utils

import java.io.File

trait Positioned {
  /** Number of bits used to encode the line number */
  private final val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  private final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  private final val LINE_MASK   = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  private final val COLUMN_MASK = (1 << COLUMN_BITS) - 1 

  private[this] def lineOf(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK
  private[this] def columnOf(pos: Int): Int = pos & COLUMN_MASK


  private[Positioned] var _file: Option[File] = None
  private[Positioned] var _line: Int = 0
  private[Positioned] var _col: Int = 0

  def setPos(file: File, pos: Int): this.type = {
    _line = lineOf(pos)
    _col  = columnOf(pos)
    _file = Some(file)

    this
  }

  def hasPosition = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _line = other._line
    _col  = other._col
    _file = other._file

    this
  }


  def file = _file.get
  def line = _line
  def col  = _col

  def position: String = {
    if (hasPosition) {
      file.getPath+":"+line+":"+col
    } else {
      "?:?"
    }
  }
}

case object NoPosition extends Positioned
