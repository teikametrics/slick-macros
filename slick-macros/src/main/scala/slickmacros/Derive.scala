package slickmacros

import shapeless.{::, Generic, HNil, Lazy}
import slick.ast.{BaseTypedType, TypedType}
import slick.jdbc.{
  GetResult,
  JdbcType,
  PositionedParameters,
  PostgresProfile,
  SetParameter
}
import slick.lifted.Isomorphism

import scala.reflect.ClassTag

trait Derive[A] {
  type Repr
  def getResult: GetResult[A]
  def getResultOpt: GetResult[Option[A]]
  def setParameter: SetParameter[A]
  def setParameterOpt: SetParameter[Option[A]]
  def baseColumnType: BaseTypedType[A] with TypedType[A] with JdbcType[A]

  // Slick doesn't really intend for Isomorphism to be used as a wrapping type, but we use it to be able to
  // declare typed column extension methods for Isomorphism-mapped types
  def isomorphism: Isomorphism[A, Repr]
}

object Derive {
  type Aux[A, R] = Derive[A] { type Repr = R }

  class Partial[A] {

    def generic[Repr](
        implicit gen: Lazy[Generic.Aux[A, Repr :: HNil]],
        gr: GetResult[Repr],
        grOpt: GetResult[Option[Repr]],
        sp: SetParameter[Repr],
        spOpt: SetParameter[Option[Repr]],
        bct: PostgresProfile.api.BaseColumnType[Repr],
        ct: ClassTag[A]
    ): Derive.Aux[A, Repr] =
      iso[A, Repr](
        r => gen.value.from(r :: HNil),
        a => gen.value.to(a).head
      )

  }

  def iso[A, Repr](
      from: Repr => A,
      to: A => Repr
  )(
      implicit
      gr: GetResult[Repr],
      grOpt: GetResult[Option[Repr]],
      sp: SetParameter[Repr],
      spOpt: SetParameter[Option[Repr]],
      bct: PostgresProfile.api.BaseColumnType[Repr],
      ct: ClassTag[A]
  ): Derive.Aux[A, Repr] =
    new DeriveIso[A, Repr](from, to)
}

private[slickmacros] class DeriveIso[A, R](
    from: R => A,
    to: A => R
)(
    implicit
    gr: GetResult[R],
    grOpt: GetResult[Option[R]],
    sp: SetParameter[R],
    spOpt: SetParameter[Option[R]],
    bct: PostgresProfile.api.BaseColumnType[R],
    ct: ClassTag[A]
) extends Derive[A] {
  override type Repr = R

  def getResult: GetResult[A] =
    gr.andThen(from)

  def getResultOpt: GetResult[Option[A]] =
    grOpt.andThen(_.map(from))

  def setParameter: SetParameter[A] =
    (a: A, pp: PositionedParameters) => sp.apply(to(a), pp)

  def setParameterOpt: SetParameter[Option[A]] =
    (a: Option[A], pp: PositionedParameters) => spOpt.apply(a.map(to), pp)

  def baseColumnType: BaseTypedType[A] with TypedType[A] with JdbcType[A] =
    PostgresProfile.api.MappedColumnType
      .base[A, R](to, from)

  def isomorphism: Isomorphism[A, Repr] = new Isomorphism[A, Repr](to, from)
}
