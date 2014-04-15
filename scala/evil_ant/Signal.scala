package evil_ant

class SwitchSignal(oneOff: Boolean) extends IEvent(oneOff) with Absorber[SwitchSignal, SwitchSet] {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() {
    isOn.set(true)
    emitters.foreach(_.activate(this))
    this.synchronized { notify() }
  }
  def turnOff() {
    isOn.set(false);
    emitters.foreach(_.deactivate(this))
    this.synchronized { notify() }
  }

  def active = isOn.get
  private def await() = wait
  private def awaitIn(milliseconds: Long) = if(milliseconds > 0) wait(milliseconds)

  override def emit(obj: AnyRef) {
    this.synchronized { while(!active) await() }
    super.emit(obj)
  }

  override def emitNow(obj: AnyRef) {
    if(active) super.emit(obj)
  }

  override def emitIn(obj: AnyRef, milliseconds: Long) {
    val beginTime = System.currentTimeMillis
    def remainingTime() = milliseconds - (System.currentTimeMillis() - beginTime)

    this.synchronized { while(!active && remainingTime() > 0) awaitIn(remainingTime()) }
    emitNow(obj)
  }
}

class SwitchSet extends Emitter[SwitchSet, SwitchSignal] {
  private[this] val active = new Set[SwitchSignal]()

  private[evil_ant] def activate(s: SwitchSignal) { active += s }
  private[evil_ant] def deactivate(s: SwitchSignal) { active -= s }

  override def +=(s: SwitchSignal) = { super.+=(s); if(s.active) activate(s); this }
  override def -=(s: SwitchSignal) = { super.-=(s); if(s.active) deactivate(s); this }

  override def emit(obj: AnyRef) = active.foreach(
    a => { if(!a.active) deactivate(a) else a.emit(obj) })
}

