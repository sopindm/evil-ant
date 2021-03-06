package evil_ant

trait Emitter[T] extends Closeable {
  protected def requireOpen = if(!isOpen) throw new ClosedEmitterException()

  def emit: Boolean = emitNow
  def emitIn(timeInMilliseconds: Long): Boolean = emitNow
  def emitNow: Boolean = { requireOpen; doEmit; true }

  protected def doEmit: Unit

  def absorbers: Iterable[T]
  def isEmpty = absorbers.isEmpty

  override def close() { super.close(); absorbers.foreach(this -= _) }

  def +=(absorber: T): Emitter[T]
  def -=(absorber: T): Emitter[T]

  final def conj(absorber: T) = +=(absorber)
  final def disj(absorber: T) = -=(absorber)
}

trait EmitterLike[This <: EmitterLike[This, A], A <: Absorber[A, This]] extends Emitter[A]
    with CloseableLike {
  self: This =>
  @volatile
  private var _absorbers = new AtomicSet[A]()
  private def absorbers_=(v: AtomicSet[A]) { _absorbers = v }
  override def absorbers = _absorbers

  override protected def doEmit { absorbers.foreach(_.callAbsorb(this)) }

  override def +=(a: A): This = { a.pushEmitter(this); pushAbsorber(a); this }
  override def -=(a: A): This = { a.popEmitter(this); popAbsorber(a); this }

  private[this] def pushAbsorber(a: A) {
    requireOpen
    absorbers += a
    if(!isOpen) absorbers -= a
  }
  private[this] def popAbsorber(a: A) { absorbers -= a }
}

trait Enableable {
  @volatile
  private var _isEnabled = true
  def enable { _isEnabled = true }
  def disable { _isEnabled = false }
  def isEnabled = _isEnabled
}

trait Absorber[This <: Absorber[This, E], E <: Emitter[This]] extends CloseableLike with Enableable {
  self: This =>

  @volatile
  private var _emitters = new AtomicSet[E]()
  private def emitter_=(v: AtomicSet[E]) { _emitters = v }
  def emitters = _emitters

  private[this] def requireOpen { if(!isOpen) throw new ClosedAbsorberException() }
  private[evil_ant] def pushEmitter(e: E) {
    requireOpen;
    emitters += e
    if(!isOpen) emitters -= e
  }
  private[evil_ant] def popEmitter(e: E) { emitters -= e }

  override def close() { super.close(); emitters.foreach(_ -= this) }

  private[evil_ant] def callAbsorb(e: E) { if(isEnabled) absorb(e) }
  protected def absorb(e: E) {}
}

class IEvent(val oneOff: Boolean) extends EmitterLike[IEvent, IHandler] with Enableable {
  def handlers = absorbers

  override protected def doEmit {
    if(!isEnabled) return
    super.doEmit
    if(oneOff) close()
  }
}

trait IHandler extends Absorber[IHandler, IEvent] {
  def events = emitters
}

class Event(oneOff: Boolean) extends IEvent(oneOff) with IHandler {
  def this() = this(false)
}

class WhenEveryEvent extends Event(true) {
  override def absorb(e: IEvent) {
    e -= this
    if(events.isEmpty) emit
  }
}

class Handler extends IHandler

