package tofu.logging
package impl

import java.util

import org.slf4j.Marker

import scala.collection.immutable.{List, Seq}

final case class ContextMarker(ctx: LoggedValue, referenceList: Seq[Marker] = List.empty) extends Marker {
  import scala.jdk.CollectionConverters._

  override def getName: String                    = "Context"
  override def add(reference: Marker): Unit       = {}
  override def remove(reference: Marker): Boolean = false
  override def hasChildren: Boolean               = referenceList.nonEmpty
  override def hasReferences: Boolean             = referenceList.nonEmpty
  override def iterator(): util.Iterator[Marker]  = referenceList.iterator.asJava
  override def contains(other: Marker): Boolean   = referenceList.contains(other)
  override def contains(name: String): Boolean    = referenceList.map(_.getName).contains(name)

  def +(marker: Marker)         = copy(ctx, marker +: referenceList)
  def addMarker(marker: Marker) = this + marker
}
