package evil_ant

class SwitchSignal(oneOff: Boolean) extends IEvent(oneOff) {
  def this() = this(false)

  private val isOn = new java.util.concurrent.atomic.AtomicBoolean(false)

  def turnOn() { this.synchronized { isOn.set(true); notify() }}
  def turnOff() { this.synchronized { isOn.set(false); notify() }}

  private def active = isOn.get
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
