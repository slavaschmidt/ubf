

/**
  * @since 07.02.2016.
  */

sealed trait UbfObject

case class UbfAtom(value: String) extends UbfObject

case class UbfBinary(value: Iterable[Byte]) extends UbfObject {
  val size = value.toSeq.length
}

object UbfBinary {
  def fromString(s: String) = apply(s.getBytes)
}

case class UbfInteger(value: Long) extends UbfObject

case class UbfString(value: String) extends UbfObject

case class UbfList(value: UbfObject*) extends UbfObject {
  def addItem(item: UbfObject) = UbfList(value.toList :+ item:_*)
}

case class UbfTuple(value: UbfObject*) extends UbfObject

