package evil_ant

import scala.collection.immutable.Set

trait Closeable extends java.io.Closeable {
  def close() {}
}

class Event extends Closeable {
  private var _handlers = Set[Handler]()
  private def handlers_=(v: Set[Handler]) { _handlers = v }
  def handlers = _handlers

  def +=(h: Handler): Event = { h.pushEvent(this); pushHandler(h); this }
  def -=(h: Handler): Event = { h.popEvent(this); popHandler(h); this }

  def conj(h: Handler) = +=(h)
  def disj(h: Handler) = -=(h)

  private[evil_ant] def pushHandler(h: Handler) { handlers += h }
  private[evil_ant] def popHandler(h: Handler) { handlers -= h }

  override def close() { handlers.foreach(this -= _) }

  def emit(value: AnyRef) { handlers.foreach(_.absorb(this, value)) }
}

class Handler extends Closeable {
  private var _events = Set[Event]()
  private def events_=(v: Set[Event]) { _events = v }
  def events = _events

  private[evil_ant] def pushEvent(e: Event) { events += e }
  private[evil_ant] def popEvent(e: Event) { events -= e }

  override def close() { events.foreach(_ -= this) }

  def absorb(e: Event, value: AnyRef) {}
}
