package evil_ant

trait BlockingEmitter[This <: Emitter[This, T], T <: Absorber[T, This]] extends Emitter[This, T] { self: This =>
  def active: Boolean

  protected def signal() { this.synchronized { notify() } }

  def await() = wait()
  def await(milliseconds: Long) = if(milliseconds > 0) wait(milliseconds)

  override def emit(obj: AnyRef) {
    this.synchronized { while(!active) await() }
    super.emit(obj)
  }

  override def emitIn(obj: AnyRef, milliseconds: Long) {
    val startTime = System.currentTimeMillis
    def remainingTime() = milliseconds - (System.currentTimeMillis() - startTime)

    this.synchronized { while(!active && remainingTime() > 0) await(remainingTime()) }
    super.emitIn(obj, milliseconds)
  }

  override def emitNow(obj: AnyRef) = if(active) super.emitNow(obj)
}

abstract class Signal[This <: Signal[This, Set], Set <: SignalSet[Set, This]](oneOff: Boolean)
    extends IEvent(oneOff)
    with BlockingEmitter[IEvent, IHandler]
    with Absorber[This, Set] {
  self: This =>

  protected def activate() = emitters.foreach(_.activate(this))
  protected def deactivate() = emitters.foreach(_.deactivate(this))

  override def active = isEnabled

  override def enable { super.enable; if(active) activate() }
  override def disable { super.disable; if(active) deactivate() }
}

trait SignalSet[This <: SignalSet[This, T], T <: Signal[T, This] with Absorber[T, This]]
    extends BlockingEmitter[This, T] {
  self: This =>

  private[evil_ant] def activate(s: T)
  private[evil_ant] def deactivate(s: T)

  override def +=(s: T) = { super.+=(s); if(s.active) activate(s); this }
  override def -=(s: T) = { super.-=(s); if(s.active) deactivate(s); this }
}

class SwitchSignal(oneOff: Boolean) extends Signal[SwitchSignal, SwitchSet](oneOff)
    with Absorber[SwitchSignal, SwitchSet] {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { isOn.set(true); activate(); signal() }
  def turnOff() { isOn.set(false); deactivate() }

  override def active = super.active && isOn.get

  override def absorb( e: SwitchSet, obj: AnyRef) = if(!active) turnOff() else doEmit(obj)
}

class SwitchSet extends SignalSet[SwitchSet, SwitchSignal] {
  private[this] val active = new Set[SwitchSignal]()

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def active() = !active.isEmpty || (active.isEmpty && absorbers.isEmpty)

  override def doEmit(obj: AnyRef) = active.foreach(_.callAbsorb(this, obj))
}

class TimerSignal(val timeout: Long, circular: Boolean, oneOff: Boolean)
    extends Signal[TimerSignal, TimerSet](oneOff) {
  def this(timeout: Long, circular: Boolean) = this(timeout, circular, false)
  def this(timeout: Long) = this(timeout, false, false)

  @volatile
  private var started: Boolean = false

  @volatile
  private var finishTime: Long = 0
  private def currentTime() = System.currentTimeMillis()

  def remaining = finishTime - currentTime()

  def start { finishTime = currentTime() + timeout; started = true }
  def stop { finishTime = currentTime(); started = false; signal() }

  override def await() = if(started) super.await(remaining)
  override def await(time: Long) = if(started) super.await(scala.math.min(time, remaining))

  override def active = !started || currentTime() >= finishTime

  override def doEmit(obj: AnyRef) = if(started) { super.doEmit(obj); if(circular) start else stop }
}

class TimerSet extends SignalSet[TimerSet, TimerSignal] {
  override def active = true

  private[evil_ant] def activate(s: TimerSignal) {}
  private[evil_ant] def deactivate(s: TimerSignal) {}
}

