package evil_ant;
import java.util.concurrent.atomic.AtomicReference;
import scala.annotation.tailrec
import scala.collection.immutable.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;

class Atom[T](obj: T) {
  private val _value = new AtomicReference[T](obj)

  def deref = _value.get

  @tailrec
  final def update(fn: T => T):T = {
    val source = deref
    val updated = fn(source)
    if(_value.compareAndSet(source, updated))
      updated
    else
      update(fn)
  }

  @tailrec
  final def updateAndGet[U](fn: T => (T, U)):U = {
    val source = deref
    val (updated, result) = fn(source)

    if(_value.compareAndSet(source, updated)) result else updateAndGet(fn)
  }
}

final class AtomicSet[T] extends Atom[Set[T]](Set[T]())
    with scala.collection.mutable.Set[T] {
  override def +=(e: T) = { update(_ + e); this }
  override def -=(e: T) = { update(_ - e); this }
  override def contains(e: T) = deref.contains(e)
  override def iterator = deref.iterator

  override def isEmpty = deref.isEmpty
}

final class AtomicMap[T, U](implicit val ordering: Ordering[T])
    extends Atom[TreeMap[T, U]](TreeMap[T, U]())
    with scala.collection.mutable.Map[T, U] {
  override def get(key: T) = deref.get(key)
  override def head = deref.head

  override def +=(kv: (T, U)) = { update(_ + kv); this }
  override def -=(key: T) = { update(_ - key); this }

  override def iterator = deref.iterator

  override def isEmpty = deref.isEmpty
}
