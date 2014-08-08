package evil_ant
import java.nio.channels.SelectableChannel
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import scala.collection.JavaConversions._

final class SelectorSignal(channel: SelectableChannel, operation: Integer, oneOff: Boolean)
    extends Signal[SelectorSignal, SelectorSet](oneOff) {
  def this(channel: SelectableChannel, operation: Integer) = this(channel, operation, false)

  override def absorb(e: SelectorSet) = doEmit

  private[evil_ant] def register(selector: Selector) = {
    val key = channel.register(selector, operation, this)
    if(!isEnabled) key.interestOps(0)
  }

  private[evil_ant] def cancel(selector: Selector) = { 
    val key = channel.keyFor(selector)
    if(key != null) key.cancel()
  }

  private[evil_ant] def activateKey(selector: Selector) = {
    val key = channel.keyFor(selector)
    if(key != null) key.interestOps(operation)
  }
  private[evil_ant] def deactivateKey(selector: Selector) = {
    val key = channel.keyFor(selector)
    if(key != null) key.interestOps(0)
  }

  private[this] def _emit(emitter: SelectorSet => Boolean): Boolean = {
    requireOpen
    if(!isEnabled) return false

    val set = new SelectorSet
    try {
      set += this
      emitter(set)
    }
    finally set.close()
  }

  override def emit = _emit((s: SelectorSet) => s.emit)
  override def emitIn(time: Long) = _emit(_.emitIn(time))
  override def emitNow = _emit(_.emitNow)
}

final class SelectorSet extends SignalSet[SelectorSignal] with CloseableLike{
  override def close() {
    if(!isOpen) return
    super.close()
    absorbers.foreach(_.popEmitter(this))
    selector.close()
  }

  override def isOpen = selector.isOpen

  private[this] val selector = Selector.open()

  private[evil_ant] override def activate(s: SelectorSignal) { s.activateKey(selector) }
  private[evil_ant] override def deactivate(s: SelectorSignal) { s.deactivateKey(selector) }

  override def absorbers = selector.keys.view.map(_.attachment.asInstanceOf[SelectorSignal])

  override def +=(s: SelectorSignal) = { requireOpen; s.register(selector); s.pushEmitter(this); this }
  override def -=(s: SelectorSignal) = { s.cancel(selector); s.popEmitter(this); this }

  override def ready = !selector.selectedKeys().isEmpty

  override def signal() { selector.wakeup() }

  override def await() = selector.select()
  override def await(time: Long) = if(time > 0) selector.select(time)

  override def emitNow = { selector.selectNow(); super.emitNow }

  private def emit1(attachment: AnyRef) = attachment match {
    case selector: SelectorSignal => selector.callAbsorb(this)
    case _ => throw new IllegalArgumentException
  }

  override def doEmit {
    val keys = selector.selectedKeys()
    keys.synchronized {
      val iterator = keys.iterator()
      while(iterator.hasNext) {
        emit1(iterator.next().attachment)
        iterator.remove()
      }
    }
  }
}

