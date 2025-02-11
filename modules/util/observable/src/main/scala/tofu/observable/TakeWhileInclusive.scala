package tofu.observable

import monix.execution.Ack
import monix.execution.Ack.Stop
import monix.reactive.observers.Subscriber

import scala.concurrent.Future
import scala.util.control.NonFatal
import monix.execution.Scheduler

//shameless copy of monix.reactive.internal.operators.TakeByPredicateOperator
private[observable] final case class TakeWhileInclusive[A](p: A => Boolean, out: Subscriber[A]) extends Subscriber[A] {
  implicit val scheduler: Scheduler = out.scheduler
  private[this] var isActive        = true

  def onNext(elem: A): Future[Ack] = {
    if (!isActive) Stop
    else {
      // Protects calls to user code from within an operator
      var streamError = true
      try {
        val isValid = p(elem)
        streamError = false

        if (isValid) out.onNext(elem)
        else {
          isActive = false
          out
            .onNext(elem)
            .map { _ =>
              out.onComplete()
              Stop
            }
        }
      } catch {
        case NonFatal(ex) if streamError =>
          onError(ex)
          Stop
      }
    }
  }

  def onComplete() =
    if (isActive) {
      isActive = false
      out.onComplete()
    }

  def onError(ex: Throwable) =
    if (isActive) {
      isActive = false
      out.onError(ex)
    }
}
