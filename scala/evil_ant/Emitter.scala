package evil_ant

trait Closeable extends java.io.Closeable {
  @volatile
  private[this] var _isOpen = true

  def isOpen = _isOpen
  def close() { _isOpen = false }
}

class ClosedEmitterException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}

class ClosedAbsorberException(msg: String) extends RuntimeException(msg) {
  def this() = this("")
}

trait Emittable extends Closeable {
  def emit(value: AnyRef) = emitNow(value)
  def emitIn(value: AnyRef, timeInMilliseconds: Long) = emitNow(value)
  def emitNow(value: AnyRef) = doEmit(value)

  protected def doEmit(value: AnyRef) {}
}

trait Emitter[This <: Emitter[This, A], A <: Absorber[A, This]] extends Emittable {
  self: This =>

  @volatile
  private var _absorbers = new AtomicSet[A]()
  private def absorbers_=(v: AtomicSet[A]) { _absorbers = v }
  def absorbers = _absorbers

  def +=(a: A): This = { a.pushEmitter(this); pushAbsorber(a); this }
  def -=(a: A): This = { a.popEmitter(this); popAbsorber(a); this }

  private[this] def requireOpen { if(!isOpen) throw new ClosedEmitterException() }
  private[this] def pushAbsorber(a: A) {
    requireOpen
    absorbers += a
    if(!isOpen) absorbers -= a
  }
  private[this] def popAbsorber(a: A) { absorbers -= a }

  def conj(a: A) = +=(a)
  def disj(a: A) = -=(a)

  override def emitNow(value: AnyRef) = { requireOpen; doEmit(value ) }
  override protected def doEmit(value: AnyRef) { absorbers.foreach(_.callAbsorb(this, value)) }

  override def close() { super.close; absorbers.foreach(this -= _) }
}

trait Absorber[This <: Absorber[This, E], E <: Emitter[E, This]] extends Closeable { self: This =>
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

  @volatile
  private var _isEnabled = true
  def enable { _isEnabled = true }
  def disable { _isEnabled = false }
  def isEnabled = _isEnabled

  private[evil_ant] def callAbsorb(e: E, value: AnyRef) { if(isEnabled) absorb(e, value) }
  protected def absorb(e: E, value: AnyRef) {}
}

trait Attachable extends Emittable {
  @volatile
  private var _attachment:AnyRef = null

  def attach(obj: AnyRef) { _attachment = obj }
  def attachment = _attachment

  override def close() { super.close(); _attachment = null }
  override protected def doEmit(value: AnyRef) {
    super.doEmit(if(attachment != null) attachment else value) }
}

trait OneOffable extends Emittable {
  val oneOff: Boolean 
  override protected def doEmit(value: AnyRef) {
    super.doEmit(value); if(oneOff) close() }
}

class IEvent(override val oneOff: Boolean) extends Emitter[IEvent, IHandler]
  with Attachable with OneOffable {
  def handlers = absorbers
}

trait IHandler extends Absorber[IHandler, IEvent] {
  def events = emitters
}

class Event(oneOff: Boolean) extends IEvent(oneOff) with IHandler {
  def this() = this(false)
}

class Handler extends IHandler

