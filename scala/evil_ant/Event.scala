package evil_ant

import scala.collection.immutable.Set

trait Closeable extends java.io.Closeable {
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

class Event extends Closeable {
  private var _handlers = Set[Handler]()
  private def handlers_=(v: Set[Handler]) { _handlers = v }
  def handlers = _handlers

  def +=(h: Handler): Event = { h.pushEvent(this); pushHandler(h); this }
  def -=(h: Handler): Event = { h.popEvent(this); popHandler(h); this }

  def conj(h: Handler) = +=(h)
  def disj(h: Handler) = -=(h)

  private[this] def requireOpen { if(!isOpen) throw new ClosedEmitterException() }
  private[evil_ant] def pushHandler(h: Handler) { requireOpen; handlers += h }
  private[evil_ant] def popHandler(h: Handler) { handlers -= h }

  override def close() { super.close(); handlers.foreach(this -= _) }

  def emit(value: AnyRef) { requireOpen; handlers.foreach(_.absorb(this, value)) }
}

class Handler extends Closeable {
  private var _events = Set[Event]()
  private def events_=(v: Set[Event]) { _events = v }
  def events = _events

  private[this] def requireOpen { if(!isOpen) throw new ClosedAbsorberException() }
  private[evil_ant] def pushEvent(e: Event) { requireOpen; events += e }
  private[evil_ant] def popEvent(e: Event) { events -= e }

  override def close() { super.close(); events.foreach(_ -= this) }

  def absorb(e: Event, value: AnyRef) {}
}
