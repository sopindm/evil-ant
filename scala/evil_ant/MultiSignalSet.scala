package evil_ant
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class MultiSignalSet extends Emitter[ISignal] {
  val switches = new SwitchSet
  val timers = new TimerSet
  val selectors = new SelectorSet

  override def close() { switches.close(); timers.close(); selectors.close() }
  override def isOpen = switches.isOpen && timers.isOpen && selectors.isOpen

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
    signal match {
      case switch: SwitchSignal => switches -= switch
      case timer: TimerSignal => timers -= timer
      case selector: SelectorSignal => selectors -= selector
      case _ => throw new IllegalArgumentException
    }
    this
  }

  override def absorbers = switches.absorbers ++ timers.absorbers ++ selectors.absorbers

  private[this] def _emit(obj: AnyRef, select: (AnyRef => Boolean), switch: (AnyRef => Boolean)) =
    if(switches.isEmpty) select(obj)
    else if(selectors.isEmpty) switch(obj)
    else if(_emitNow(obj)) true
    else _emitAll(obj, select, switch)

  private[this] def _emitNow(obj: AnyRef) = {
    val selectorsReady = selectors.emitNow(obj)
    val switchesReady = switches.emitNow(obj)

    if(selectorsReady || switchesReady) { timers.emitNow(obj); true } else false
  }

  private[this] def _emitAll(obj: AnyRef, select: (AnyRef => Boolean), switch: (AnyRef => Boolean)) = {
    val selection = future { try select(obj) finally switches.signal() }

    val switched = try switch(obj) finally selectors.signal()
    val selected = Await.result(selection, Duration.Inf)

    if(switched || selected) true else timers.emitNow(obj)
  }

  override def emit(obj: AnyRef) = timers.timeout match {
    case Some(time) => _emit(obj, selectors.emitIn(_, time), switches.emitIn(_, time))
    case None => _emit(obj, selectors.emit(_), switches.emit(_))
  }

  override def emitIn(obj: AnyRef, millis: Long) = {
    val timeout = timers.timeout match {
      case Some(time) => scala.math.min(time, millis)
      case None => millis
    }
    _emit(obj, selectors.emitIn(_, timeout), switches.emitIn(_, timeout))
  }

  override def emitNow(obj: AnyRef) = {
    val switched = switches.emitNow(obj)
    val selected = selectors.emitNow(obj)
    val timedOut = timers.emitNow(obj)
    switched || selected || timedOut
  }

  override protected def doEmit(value: AnyRef) { throw new UnsupportedOperationException }
}
