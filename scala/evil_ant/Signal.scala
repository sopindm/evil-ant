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

class SwitchSignal(oneOff: Boolean) extends Signal[SwitchSignal, SwitchSet](oneOff)
    with Absorber[SwitchSignal, SwitchSet] {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { isOn.set(true); activate(); signal() }
  def turnOff() { isOn.set(false); deactivate() }

  override def ready = super.ready && isOn.get
  override def active = ready

  override def absorb( e: SwitchSet, obj: AnyRef) = if(!active) turnOff() else emitNow(obj)
}

class SwitchSet extends SignalSet[SwitchSet, SwitchSignal] {
  private[this] val active = new AtomicSet[SwitchSignal]

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def ready = !active.isEmpty || (active.isEmpty && absorbers.isEmpty)

  override def doEmit(obj: AnyRef) = active.foreach(_.callAbsorb(this, obj))
}

class TimerSignal(val timeout: Long, circular: Boolean, oneOff: Boolean)
    extends Signal[TimerSignal, TimerSet](oneOff) {
  def this(timeout: Long, circular: Boolean) = this(timeout, circular, false)
  def this(timeout: Long) = this(timeout, false, false)

  @volatile
  private var started: Boolean = false

  @volatile
  private var _finishTime: Long = 0
  def finishTime = _finishTime

  private def currentTime() = System.currentTimeMillis()

  def remaining = finishTime - currentTime()

  def start { _finishTime = currentTime() + timeout; started = true; activate() }
  def stop { deactivate(); _finishTime = currentTime(); started = false; signal() }

  override def await() = if(active) super.await(remaining)
  override def await(time: Long) = if(active) super.await(scala.math.min(time, remaining))

  override def active = started
  override def ready = !active || currentTime() >= finishTime

  override def doEmit(obj: AnyRef) = if(ready && active) { super.doEmit(obj); if(circular) start else stop }
}

final class TimerSet extends SignalSet[TimerSet, TimerSignal] {
  val timeouts = new AtomicMap[Long, Set[TimerSignal]]
  def timeout =
    if(!timeouts.isEmpty)
      timeouts.head._1 - System.currentTimeMillis()
    else 0

  override def await() =
    if(timeouts.isEmpty) super.await()
    else super.await(timeout)

  override def await(millis: Long) =
    if(timeouts.isEmpty) super.await(millis)
    else super.await(scala.math.min(millis, timeout))

  override def ready = !timeouts.isEmpty && (timeout <= 0)

  private[evil_ant] override def activate(s: TimerSignal) {
    val finishTime = s.finishTime
    timeouts.update(ts => ts.get(finishTime) match {
      case Some(signals) => ts + (finishTime -> (signals + s))
      case None => ts + (finishTime -> Set[TimerSignal](s))
    })
    signal()
  }

  private[evil_ant] override def deactivate(s: TimerSignal) {
    val finishTime = s.finishTime
    timeouts.update(ts => ts.get(finishTime) match {
      case Some(signals) => { 
        val updatesSignals = signals - s
        if(updatesSignals.isEmpty) ts - finishTime else ts + (finishTime -> updatesSignals)}
      case None => ts
    })
  }

  @tailrec
  override final def doEmit(obj: AnyRef) =
    if(ready) {
      val head = timeouts.updateAndGet(ts => (ts.tail, ts.head))
      if(head._1 <= System.currentTimeMillis()) {
        head._2.foreach(_.callAbsorb(this, obj))
        doEmit(obj)
      }
      else
        head._2.foreach(activate(_))
    }
}
