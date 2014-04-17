package evil_ant

trait BlockingEmitter[This <: Emitter[This, T], T <: Absorber[T, This]] extends Emitter[This, T] { self: This =>
  def active: Boolean

  protected def signal() { this.synchronized { notify() } }

  def await() = wait()
  def await(milliseconds: Long) = if(milliseconds > 0) wait(milliseconds)

  override def emit(obj: AnyRef) {
    this.synchronized { while(!active) await() }
    doEmit(obj)
  }

  override def emitIn(obj: AnyRef, milliseconds: Long) {
    val startTime = System.currentTimeMillis
    def remainingTime() = milliseconds - (System.currentTimeMillis() - startTime)

    this.synchronized { while(!active && remainingTime() > 0) await(remainingTime()) }
    emitNow(obj)
  }

  override def emitNow(obj: AnyRef) = if(active) doEmit(obj)
}

abstract class Signal(oneOff: Boolean) extends IEvent(oneOff) with BlockingEmitter[IEvent, IHandler]

class SwitchSignal(oneOff: Boolean) extends Signal(oneOff) with Absorber[SwitchSignal, SwitchSet] {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() {
    isOn.set(true)
    emitters.foreach(_.activate(this))
    signal()
  }

  def turnOff() {
    isOn.set(false);
    emitters.foreach(_.deactivate(this))
  }

  override def enable() {
    super.enable;
    if(isOn.get) emitters.foreach(_.activate(this)) }

  override def disable() {
    super.disable;
    if(isOn.get) emitters.foreach(_.deactivate(this)) }

  override def active = isOn.get && isEnabled

  override def absorb( e: SwitchSet, obj: AnyRef) = if(!active) turnOff() else emit(obj)
}

trait SignalSet[This <: SignalSet[This, T], T <: Signal with Absorber[T, This]]
    extends BlockingEmitter[This, T] {
  self: This =>

  private[evil_ant] def activate(s: T)
  private[evil_ant] def deactivate(s: T)

  override def +=(s: T) = { super.+=(s); if(s.active) activate(s); this }
  override def -=(s: T) = { super.-=(s); if(s.active) deactivate(s); this }
}

class SwitchSet extends SignalSet[SwitchSet, SwitchSignal] {
  private[this] val active = new Set[SwitchSignal]()

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def active() = !active.isEmpty || (active.isEmpty && absorbers.isEmpty)
}

