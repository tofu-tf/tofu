package tofu.logging.impl

import tofu.logging.impl.TofuDefaultWithZIOContextImpl.makeZioContext
import tofu.logging.zlogs.ZLogAnnotation
import tofu.logging.{Loggable, LoggedValue}
import zio.{Clock, FiberRef, LogSpan, UIO}

class TofuDefaultWithZIOContextImpl extends TofuDefaultContextImpl {
  override def getCtx: UIO[LoggedValue] =
    for {
      tofuCtx      <- super.getCtx
      logSpans     <- FiberRef.currentLogSpan.get
      zAnnotations <- FiberRef.currentLogAnnotations.get
      now          <- Clock.instant
    } yield new ComposedLoggedValue(
      tofuCtx :: makeZioContext(now.toEpochMilli, zAnnotations, logSpans)
    )
}

object TofuDefaultWithZIOContextImpl {

  val zioAnnotationsLoggable: Loggable[Map[String, String]] =
    Loggable[Map[String, String]].named(ZLogAnnotation.ZioAnnotationsKey)

  // now: Long - milliseconds
  def zioSpansLoggable(now: Long): Loggable[List[LogSpan]] =
    Loggable[Map[String, Long]]
      .named(ZLogAnnotation.ZioLogSpansKey)
      .contramap { list =>
        list.map(span => (span.label, now - span.startTime)).toMap
      }

  def makeZioContext(now: Long, zioAnnotations: Map[String, String], zioSpans: List[LogSpan]): List[LoggedValue] = {
    zioSpans.headOption.map(_ => zioSpansLoggable(now).loggedValue(zioSpans)) ::
      zioAnnotations.headOption.map(_ => zioAnnotationsLoggable.loggedValue(zioAnnotations)) ::
      Nil
  }.flatten
}
