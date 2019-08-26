package slickmacros

import slick.ast.TypedType
import slick.jdbc.GetResult
import slick.lifted.Isomorphism

case class UserId(value: Int) extends AnyVal

@AutoSlick.Mixin[UserId]
trait UserIdInstances

@AutoSlick
case class UserName(value: String) extends AnyVal

@AutoSlick // With companion object
case class Age(value: Int) extends AnyVal

object Age {
  def one = Age(1)
}

case class Comment(value: String) extends AnyVal

@AutoSlick.Mixin[Comment]
trait CommentInstances

@AutoSlick.Mixin[UserId]
@AutoSlick.Mixin[Comment]
trait StackedMixinInstances

object CheckStackedMixin extends StackedMixinInstances {
  val getUserId: GetResult[UserId] = implicitly[GetResult[UserId]]
  val getComment: GetResult[Comment] = implicitly[GetResult[Comment]]
}

/** If this compiles, SlickInstances works. Trait in a separate file to lower debug noise */
object CheckSlickInstances extends UserIdInstances {

  // Intellij thinks that the implicit isn't satisfied, I don't know how to fix that yet :(
  val getUserId: GetResult[UserId] = implicitly[GetResult[UserId]]

  val getUserName: GetResult[UserName] = implicitly[GetResult[UserName]]

  val typedType: TypedType[UserId] = implicitly[TypedType[UserId]]

}

object CheckMixins extends UserIdInstances with CommentInstances {
  val getUserId: GetResult[UserId] = implicitly[GetResult[UserId]]
  val getComment: GetResult[Comment] = implicitly[GetResult[Comment]]

}

class Wrapped(val value: String)

@AutoSlick.Iso[Wrapped, String](new Wrapped(_), _.value)
trait WrappedInstances

object CheckIsoMacro extends WrappedInstances {
  val getWrapped: GetResult[Wrapped] = implicitly[GetResult[Wrapped]]
}

@AutoSlick.Mixin[UserId]
trait CheckImporting {
  import slick.lifted.Rep
  def foo: Rep[UserId] // Rep should not collide
}

// TODO Mixin[prefixed.Foo]

@AutoSlick.Mixin[UserId]
@AutoSlick.Iso[Wrapped, String](new Wrapped(_), _.value)
object CheckIsomorphism {
  // TODO
  // val x: Isomorphism[UserId, Int] = implicitly[Isomorphism[UserId, Int]]
  
  val y: Isomorphism[Wrapped, String] = implicitly[Isomorphism[Wrapped, String]]
}
