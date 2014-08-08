package evil_ant

final class SwitchSignal(oneOff: Boolean) extends Signal[SwitchSignal, SwitchSet](oneOff) {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { isOn.set(true); activate(); signal() }
  def turnOff() { isOn.set(false); deactivate() }
  def turn(on: Boolean) = if(on) turnOn() else turnOff()

  override def ready = super.ready && isOn.get
  override def active = ready

  override def absorb( e: SwitchSet) = if(!active) turnOff() else emitNow
}

final class SwitchSet extends SignalSetLike[SwitchSet, SwitchSignal] {
  private[this] val active = new AtomicSet[SwitchSignal]

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def ready = !active.isEmpty

  override def doEmit = active.foreach(_.callAbsorb(this))
}

