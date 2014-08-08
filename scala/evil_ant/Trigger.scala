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

  override def emitNow = {
    requireOpen
    if(active.isEmpty) false else { doEmit; true }
  }

  override def doEmit = {
    while(!active.isEmpty) {
      val traversing = active
      active = List[TriggerSignal]()

      traversing.foreach(_.callAbsorb(this))
    }
  }
}
