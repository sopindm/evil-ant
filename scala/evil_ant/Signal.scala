package evil_ant

trait BlockingEmitter[T] extends Emitter[T] {
  def ready: Boolean

  def signal() { this.synchronized { notifyAll() } }

  def await() = wait()
  def await(milliseconds: Long) = if(milliseconds > 0) wait(milliseconds)

  override def emit: Boolean = {
    this.synchronized { if(!ready) await()}
    super.emit
  }

  override def emitIn(milliseconds: Long): Boolean = {
    val startTime = System.currentTimeMillis
    def remainingTime() = milliseconds - (System.currentTimeMillis() - startTime)

    this.synchronized { if(!ready && remainingTime() > 0) await(remainingTime()) }
    if(ready) super.emitIn(milliseconds) else false
  }

  override def emitNow = if(ready) super.emitNow else false
}

abstract class ISignal(oneOff: Boolean) extends IEvent(oneOff)
    with BlockingEmitter[IHandler] with EmitterLike[IEvent, IHandler] {
  protected def activate()
  protected def deactivate()

  def active = isEnabled
  override def ready = isEnabled
}

abstract class Signal[This <: Signal[This, Set], Set <: SignalSet[This]](oneOff: Boolean)
    extends ISignal(oneOff) with Absorber[This, Set] {
  self: This =>

  override protected def activate() = emitters.foreach(_.activate(this))
  override protected def deactivate() = emitters.foreach(_.deactivate(this))

  override def enable { super.enable; if(active) activate() }
  override def disable { deactivate(); super.disable; if(active) activate() }

  override def absorb(e: Set) = emitNow
}

abstract class SignalSet[T] extends BlockingEmitter[T] {
  private[evil_ant] def activate(s: T)
  private[evil_ant] def deactivate(s: T)
}

abstract class SignalSetLike[This <: SignalSetLike[This, T], T <: Signal[T, This]]
    extends SignalSet[T] with EmitterLike[This, T] {
  self: This =>

  override def +=(s: T) = { super.+=(s); if(s.active) activate(s); this }
  override def -=(s: T) = { super.-=(s); if(s.active) deactivate(s); this }
}
