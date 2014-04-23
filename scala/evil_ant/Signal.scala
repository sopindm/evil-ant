package evil_ant

trait BlockingEmitter[T] extends Emitter[T] {
  def ready: Boolean

  def signal() { this.synchronized { notify() } }

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

abstract class Signal[This <: Signal[This, Set], Set <: SignalSet[This]](oneOff: Boolean)
    extends IEvent(oneOff)
    with BlockingEmitter[IHandler] with EmitterLike[IEvent, IHandler]
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
