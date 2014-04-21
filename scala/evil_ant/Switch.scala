package evil_ant

final class SwitchSignal(oneOff: Boolean) extends Signal[SwitchSignal, SwitchSet](oneOff)
    with Absorber[SwitchSignal, SwitchSet] {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { isOn.set(true); activate(); signal() }
  def turnOff() { isOn.set(false); deactivate() }

  override def ready = super.ready && isOn.get
  override def active = ready

  override def absorb( e: SwitchSet, obj: AnyRef) = if(!active) turnOff() else emitNow(obj)
}

final class SwitchSet extends SignalSet[SwitchSet, SwitchSignal] {
  private[this] val active = new AtomicSet[SwitchSignal]

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def ready = !active.isEmpty || (active.isEmpty && absorbers.isEmpty)

  override def doEmit(obj: AnyRef) = active.foreach(_.callAbsorb(this, obj))
}

