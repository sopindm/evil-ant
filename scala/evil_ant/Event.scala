package evil_ant

trait Closeable extends java.io.Closeable {
  @volatile
  private[this] var _isOpen = true

  def isOpen = _isOpen
  def close() { _isOpen = false }
}

class ClosedEmitterException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}

class ClosedAbsorberException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}

class Set[T] extends scala.collection.mutable.Set[T] {
  @volatile
  private [this] var _set = scala.collection.immutable.Set[T]()

  override def +=(e: T) = this.synchronized { _set += e; this }
  override def -=(e: T) = this.synchronized { _set -= e; this }
  override def contains(e: T) = _set.contains(e)
  override def iterator = _set.iterator
}

class Event extends Closeable {
  @volatile
  private var _handlers = new Set[Handler]()
  private def handlers_=(v: Set[Handler]) { _handlers = v }
  def handlers = _handlers

  def +=(h: Handler): Event = { h.pushEvent(this); pushHandler(h); this }
  def -=(h: Handler): Event = { h.popEvent(this); popHandler(h); this }

  def conj(h: Handler) = +=(h)
  def disj(h: Handler) = -=(h)

  private[this] def requireOpen { if(!isOpen) throw new ClosedEmitterException() }
  private[evil_ant] def pushHandler(h: Handler) {
    requireOpen;
    handlers += h
    if(!isOpen) handlers -= h
  }

  private[evil_ant] def popHandler(h: Handler) { handlers -= h }

  override def close() { super.close(); handlers.foreach(this -= _) }

  def emit(value: AnyRef) { requireOpen; handlers.foreach(_.callAbsorb(this, value)) }
}

class Handler extends Closeable {
  @volatile
  private var _events = new Set[Event]()
  private def events_=(v: Set[Event]) { _events = v }
  def events = _events

  private[this] def requireOpen { if(!isOpen) throw new ClosedAbsorberException() }
  private[evil_ant] def pushEvent(e: Event) {
    requireOpen;
    events += e
    if(!isOpen) events -= e
  }
  private[evil_ant] def popEvent(e: Event) { events -= e }

  override def close() { super.close(); events.foreach(_ -= this) }

  @volatile
  private var _isEnabled = true
  def enable { _isEnabled = true }
  def disable { _isEnabled = false }
  def isEnabled = _isEnabled

  private[evil_ant] def callAbsorb(e: Event, value: AnyRef) { if(isEnabled) absorb(e, value) }
  protected def absorb(e: Event, value: AnyRef) {}
}
