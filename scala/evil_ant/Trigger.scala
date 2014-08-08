package evil_ant

final class TriggerSignal(oneOff: Boolean) extends Signal[TriggerSignal, TriggerSet](oneOff) {
  def this() = this(false)

  def touch() { activate() }
}

final class TriggerSet extends SignalSet[TriggerSignal] with EmitterLike[TriggerSet, TriggerSignal] {
  private[this] var active = List[TriggerSignal]()

  override def ready = true

  private[evil_ant] override def activate(s: TriggerSignal) { active = s +: active }
  private[evil_ant] override def deactivate(s: TriggerSignal) {}

  override def doEmit(obj: AnyRef) = {
    while(!active.isEmpty) {
      val traversing = active
      active = List[TriggerSignal]()

      traversing.foreach(_.callAbsorb(this, obj))
    }
  }
}

/*
final class SwitchSignal(oneOff: Boolean) extends Signal[SwitchSignal, SwitchSet](oneOff) {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { isOn.set(true); activate(); signal() }
  def turnOff() { isOn.set(false); deactivate() }
  def turn(on: Boolean) = if(on) turnOn() else turnOff()

  override def ready = super.ready && isOn.get
  override def active = ready

  override def absorb( e: SwitchSet, obj: AnyRef) = if(!active) turnOff() else emitNow(obj)
}

final class SwitchSet extends SignalSetLike[SwitchSet, SwitchSignal] {
  private[this] val active = new AtomicSet[SwitchSignal]

  private[evil_ant] override def activate(s: SwitchSignal) { active += s; signal() }
  private[evil_ant] override def deactivate(s: SwitchSignal) { active -= s }

  override def ready = !active.isEmpty || (active.isEmpty && absorbers.isEmpty)

  override def doEmit(obj: AnyRef) = active.foreach(_.callAbsorb(this, obj))
}*/

