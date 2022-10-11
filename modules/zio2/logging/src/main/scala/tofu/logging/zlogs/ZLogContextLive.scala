package tofu.logging.zlogs

import tofu.logging.zlogs.ZLogContextLive.ContextDump
import tofu.logging.{LogAnnotation, LoggedValue}
import zio.{FiberRef, FiberRefs, Tag, UIO, ULayer, ZLayer}

class ZLogContextLive(ref: FiberRef[ContextDump]) extends ZLogContext with ZLogContext.LoggedValuesExtractor {

  override def update[A](pair: (LogAnnotation[A], A)): UIO[Unit] =
    ref.update(_ + pair)

  override def get[A](key: LogAnnotation[A]): UIO[Option[A]] =
    ref.get.map(_.get(key).asInstanceOf[Option[A]])

  override def getOrUpdate[A](pair: (LogAnnotation[A], A)): UIO[A] = {
    val (key, value) = pair
    ref.modify[A] { dataMap =>
      if (dataMap.contains(key)) {
        (dataMap(key).asInstanceOf[A], dataMap)
      } else {
        (value, dataMap + pair)
      }
    }
  }

  override def loggedValue: UIO[LoggedValue] = ref.get.map(makeLoggedValue)

  override def loggedValueFromFiberRefs(fiberRefs: FiberRefs): Option[LoggedValue] =
    fiberRefs.get(ref).map(makeLoggedValue)

  private def makeLoggedValue(map: Map[LogAnnotation[_], Any]): LoggedValue = {
    map.map[String, LoggedValue] {
      case (annot, value) =>
        (annot.name, annot +> value.asInstanceOf[annot.Value])
    }
  }

}

object ZLogContextLive {
  val layer: ULayer[ZLogContextLive] = ZLayer.scoped(
    FiberRef.make[ContextDump](
      initial = Map.empty,
      join = (parent, child) => parent ++ child
    ).map(new ZLogContextLive(_))
  )

  val DumpTag = Tag[ContextDump]

  type ContextDump = Map[LogAnnotation[_], Any]
}
