package evil_ant
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

class MultiSignalSet extends Emitter[ISignal] {
  val switches = new SwitchSet
  val timers = new TimerSet
  val selectors = new SelectorSet

  override def close() {}
  override def isOpen = true

  override def +=(signal: ISignal) = {
    signal match {
      case switch: SwitchSignal => switches += switch
      case timer: TimerSignal => timers += timer
      case selector: SelectorSignal => selectors += selector
      case _ => throw new IllegalArgumentException
    }
    this
  }

  override def -=(signal: ISignal) = {
    /*
    signal match {
      case switch: SwitchSignal => switches -= switch
      case timer: TimerSignal => timers -= timer
      case selector: SelectorSignal => selectors -= selector
      case _ => throw new IllegalArgumentException
    }*/
    this
  }

  override def absorbers = switches.absorbers ++ timers.absorbers ++ selectors.absorbers

  private[this] def _emit(obj: AnyRef) =
    if(switches.isEmpty) selectors.emit(obj)
    else if(selectors.isEmpty) switches.emit(obj)
    else {
      val selection = future { selectors.emit(obj); switches.signal() }
      switches.emit(obj)
      selectors.signal()
    }

  private[this] def _emitIn(obj: AnyRef, time: Long) =
    if(switches.isEmpty) selectors.emitIn(obj, time)
    else if(selectors.isEmpty) switches.emitIn(obj, time)
    else {
      val selection = future { selectors.emitIn(obj, time); switches.signal() }
      switches.emitIn(obj, time)
      selectors.signal()
    }

  override def emit(obj: AnyRef) =  timers.timeout match {
    case Some(time) => { _emitIn(obj, time); timers.emitNow(obj) }
    case None => _emit(obj)
  }

  override def emitNow(obj: AnyRef) { switches.emitNow(obj); timers.emitNow(obj) }

  override protected def doEmit(value: AnyRef) { throw new UnsupportedOperationException }
}
