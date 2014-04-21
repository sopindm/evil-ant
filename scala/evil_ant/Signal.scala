package evil_ant
import scala.annotation.tailrec

trait BlockingEmitter[This <: Emitter[This, T], T <: Absorber[T, This]] extends Emitter[This, T] { self: This =>
  def ready: Boolean

  protected def signal() { this.synchronized { notify() } }

  def await() = wait()
  def await(milliseconds: Long) = if(milliseconds > 0) wait(milliseconds)

  override def emit(obj: AnyRef) {
    this.synchronized { while(!ready) await() }
    super.emit(obj)
  }

  override def emitIn(obj: AnyRef, milliseconds: Long) {
    val startTime = System.currentTimeMillis
    def remainingTime() = milliseconds - (System.currentTimeMillis() - startTime)

    this.synchronized { while(!ready && remainingTime() > 0) await(remainingTime()) }
    super.emitIn(obj, milliseconds)
  }

  override def emitNow(obj: AnyRef) = if(ready) super.emitNow(obj)
}

abstract class Signal[This <: Signal[This, Set], Set <: SignalSet[Set, This]](oneOff: Boolean)
    extends IEvent(oneOff)
    with BlockingEmitter[IEvent, IHandler]
    with Absorber[This, Set] {
  self: This =>

  protected def activate() = emitters.foreach(_.activate(this))
  protected def deactivate() = emitters.foreach(_.deactivate(this))

  def active = isEnabled
  override def ready = isEnabled

  override def enable { super.enable; if(active) activate() }
  override def disable { super.disable; if(active) deactivate() }

  override def absorb(e: Set, obj: AnyRef) = emitNow(obj)
}

trait SignalSet[This <: SignalSet[This, T], T <: Signal[T, This] with Absorber[T, This]]
    extends BlockingEmitter[This, T] {
  self: This =>

  private[evil_ant] def activate(s: T)
  private[evil_ant] def deactivate(s: T)

  override def +=(s: T) = { super.+=(s); if(s.active) activate(s); this }
  override def -=(s: T) = { super.-=(s); if(s.active) deactivate(s); this }
}
