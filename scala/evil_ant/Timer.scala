package evil_ant
import scala.annotation.tailrec

final class TimerSignal(val timeout: Long, circular: Boolean, oneOff: Boolean)
    extends Signal[TimerSignal, TimerSet](oneOff) {
  def this(timeout: Long, circular: Boolean) = this(timeout, circular, false)
  def this(timeout: Long) = this(timeout, false, false)

  @volatile
  private var started: Boolean = false

  @volatile
  private var _finishTime: Long = 0
  def finishTime = _finishTime

  private def currentTime() = System.currentTimeMillis()

  def remaining = finishTime - currentTime()

  def start { _finishTime = currentTime() + timeout; started = true; activate() }
  def stop { deactivate(); _finishTime = currentTime(); started = false; signal() }

  override def await() = if(active) super.await(remaining)
  override def await(time: Long) = if(active) super.await(scala.math.min(time, remaining))

  override def active = started
  override def ready = !active || currentTime() >= finishTime

  override def doEmit = if(ready && active) {
    super.doEmit
    if(circular) start else stop
  }
}

final class TimerSet extends SignalSetLike[TimerSet, TimerSignal] {
  val timeouts = new AtomicMap[Long, Set[TimerSignal]]
  def timeout = if(!timeouts.isEmpty)
    Some[Long](timeouts.head._1 - System.currentTimeMillis())
  else 
    None

  override def await() = timeout match {
    case Some(time) => super.await(time)
    case None => super.await()
  }

  override def await(millis: Long) = timeout match {
    case Some(time) => super.await(scala.math.min(millis, time))
    case None => super.await(millis)
  }

  override def ready = timeout match { case Some(time) if time <= 0 => true; case _ => false }

  private[evil_ant] override def activate(s: TimerSignal) {
    val finishTime = s.finishTime
    timeouts.update(ts => ts.get(finishTime) match {
      case Some(signals) => ts + (finishTime -> (signals + s))
      case None => ts + (finishTime -> Set[TimerSignal](s))
    })
    signal()
  }

  private[evil_ant] override def deactivate(s: TimerSignal) {
    val finishTime = s.finishTime
    timeouts.update(ts => ts.get(finishTime) match {
      case Some(signals) => { 
        val updatesSignals = signals - s
        if(updatesSignals.isEmpty) ts - finishTime else ts + (finishTime -> updatesSignals)}
      case None => ts
    })
  }

  @tailrec
  override final def doEmit =
    if(ready) {
      val head = timeouts.updateAndGet(ts => (ts.tail, ts.head))
      if(head._1 <= System.currentTimeMillis()) {
        head._2.foreach(_.callAbsorb(this))
        doEmit
      }
      else
        head._2.foreach(activate(_))
    }
}
