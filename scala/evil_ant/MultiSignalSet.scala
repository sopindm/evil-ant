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

  private[this] def _emit(select: => Boolean, switch: => Boolean) =
    if(switches.isEmpty) select
    else if(selectors.isEmpty) switch
    else if(_emitNow) true
    else _emitAll(select, switch)

  private[this] def _emitNow = {
    val selectorsReady = selectors.emitNow
    val switchesReady = switches.emitNow

    if(selectorsReady || switchesReady) { timers.emitNow; true } else false
  }

  private[this] def _emitAll(select: => Boolean, switch: => Boolean) = {
    val selection = future { try select finally switches.signal() }

    val switched = try switch finally selectors.signal()
    val selected = Await.result(selection, Duration.Inf)

    if(switched || selected) true else timers.emitNow
  }

  override def emit = timers.timeout match {
    case Some(time) => _emit(selectors.emitIn(time), switches.emitIn(time))
    case None => _emit(selectors.emit, switches.emit)
  }

  override def emitIn(millis: Long) = {
    val timeout = timers.timeout match {
      case Some(time) => scala.math.min(time, millis)
      case None => millis
    }
    _emit(selectors.emitIn(timeout), switches.emitIn(timeout))
  }

  override def emitNow = {
    val switched = switches.emitNow
    val selected = selectors.emitNow
    val timedOut = timers.emitNow
    switched || selected || timedOut
  }

  override protected def doEmit { throw new UnsupportedOperationException }
}
