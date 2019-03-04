package ichor.core

sealed abstract class SetBy
object SetBy {
  /** Metadata is set by the user. */
  case object User extends SetBy { override def toString: String = "SymData.User" }
  /** Metadata is set by a @flow rule. */
  case object Flow {
    /** Metadata is set by a @flow rule on itself. */
    case object Self extends SetBy { override def toString: String = "SymData.Flow.Self" }
    /** Metadata is set by a @flow rule of one of its consumers. */
    case object Consumer extends SetBy { override def toString: String = "SymData.Flow.Consumer" }
  }
  /** Metadata is set by an analysis pass. */
  case object Analysis {
    /** Metadata is set by visiting that node in an analysis pass. */
    case object Self extends SetBy { override def toString: String = "SymData.Analysis.Self" }
    /** Metadata is set by visiting some consumer in an analysis pass. */
    case object Consumer extends SetBy { override def toString: String = "SymData.Analysis.Consumer" }
  }
}
object GlobalData {
  /** Metadata is set by the user. */
  case object User extends SetBy { override def toString: String = "GlobalData.User" }
  /** Metadata is set by a @flow. */
  case object Flow extends SetBy { override def toString: String = "GlobalData.Flow" }
  /** Metadata is set by an analysis pass. */
  case object Analysis extends SetBy { override def toString: String = "GlobalData.Analysis" }
}

/** Transfer determines how metadata is transferred across Transformers.
  * Symbol metadata:
  *   Mirror - Metadata is mirrored (using its mirror rule) and explicitly transferred
  *   Remove - Metadata is dropped (explicitly removed) during symbol transformation
  *   Ignore - Nothing is explicitly done (metadata is dropped if not set by some external rule)
  *
  * Global metadata:
  *   Mirror - Nothing is explicitly done (metadata is assumed to be stable or updated explicitly)
  *   Remove - Metadata is dropped (explicitly removed) prior to transformation
  *   Ignore - Nothing is explicitly done (metadata is assumed to be stable or updated explicitly)
  */
object Transfer extends Enumeration {
  type Transfer = Value
  val Remove, Mirror, Ignore = Value

  def apply(src: SetBy): Transfer = src match {
    case SetBy.User              => Mirror
    case SetBy.Flow.Self         => Mirror
    case SetBy.Flow.Consumer     => Remove
    case SetBy.Analysis.Self     => Mirror
    case SetBy.Analysis.Consumer => Remove
    case GlobalData.User         => Mirror
    case GlobalData.Flow         => Remove
    case GlobalData.Analysis     => Remove
  }
}

/** Any kind of IR graph metadata.
  *
  * For consistency, global analysis data is dropped before transformers are run if it is Mirror or Remove.
  *
  * If you're not sure which one is right, use the SetBy subclasses instead to specify how the
  * metadata is created.
  */
abstract class Meta[T](val transfer: Transfer.Transfer) { self =>
  def this(setBy: SetBy) = this(Transfer(setBy))

  /** Defines how to copy metadata during mirroring/updating. */
  def mirror(f: Tx): T = this.asInstanceOf[T]

  def key: Class[_] = self.getClass
  override final def hashCode(): Int = key.hashCode()
}
