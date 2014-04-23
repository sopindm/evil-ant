package evil_ant

trait Closeable extends java.io.Closeable {
  def isOpen: Boolean
  def close() {}
}

trait CloseableLike extends Closeable {
  @volatile
  private[this] var _isOpen = true

  override def isOpen = _isOpen
  override def close() { super.close(); _isOpen = false }
}

class ClosedEmitterException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}

class ClosedAbsorberException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}
