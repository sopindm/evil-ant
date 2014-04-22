package evil_ant

final class SelectorSignal(channel: java.nio.channels.SelectableChannel, oneOff: Boolean)
    extends Signal[SelectorSignal, SelectorSet](oneOff) {
  def this(channel: java.nio.channels.SelectableChannel) = this(channel, false)

  override def absorb( e: SelectorSet, obj: AnyRef) = doEmit(obj)
}

final class SelectorSet extends SignalSet[SelectorSet, SelectorSignal] {
  private[evil_ant] override def activate(s: SelectorSignal) {}
  private[evil_ant] override def deactivate(s: SelectorSignal) {}

  override def ready = true
}

