package slickmacros

import slick.ast.{BaseTypedType, TypedType}
import slick.jdbc.{GetResult, JdbcType, SetParameter}
import slick.lifted.Isomorphism

import scala.reflect.macros.blackbox

/** Automatically generate slick typeclass instances for single-value wrappers
  *
  * @example {{{
  * @AutoSlick
  * case class UserId(value: Int) extends AnyVal
  *
  * case class FromOtherLibrary(value: String) extends AnyVal
  *
  * @AutoSlick.Mixin[FromOtherLibrary]
  * trait FromOtherLibraryInstances
  * }}}
  * */
class AutoSlick extends scala.annotation.StaticAnnotation {

  def macroTransform(annottees: Any*): Any =
    macro AutoSlickMacros.caseClassMacro
}

object AutoSlick {

  /** Generate a mixin trait with instances for `T`
    *
    * {{{
    *   case class UserId(value: Int) extends AnyVal
    *   @AutoSlick.Mixin[UserId]
    *   trait UserIdInstances
    *
    * }}}*/
  class Mixin[T] extends scala.annotation.StaticAnnotation {

    def macroTransform(annottees: Any*): Any =
      macro AutoSlickMacros.mixinMacro[T]
  }

  class Iso[T, Repr](
      from: Repr => T,
      to: T => Repr
  ) extends scala.annotation.StaticAnnotation {
    private val _ = (from, to) // warn-unused bypass

    def macroTransform(annottees: Any*): Any =
      macro AutoSlickMacros.isoMacro[T, Repr]
  }

}

final class AutoSlickMacros(val c: blackbox.Context) {
  import c.universe._

  def isoMacro[T, Repr](
      annottees: Tree*
  ): Tree = infoTap {
    val q"new $_.Iso[${tpFrom: TypeName}, ${tpTo: TypeName}](${fnFrom: Tree}, ${fnTo: Tree}).macroTransform(..$_)" =
      c.macroApplication

    annottees match {
      case List(
          q"..$mods trait $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        q"""
         $mods trait $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesIso(tpFrom, tpTo)(fnFrom, fnTo)}
           ..$objDefs
         }
         """
      case List(
          q"..$mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        q"""
         $mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesIso(tpFrom, tpTo)(fnFrom, fnTo)}
           ..$objDefs
         }
         """
      case bad =>
        c.abort(
          c.enclosingPosition,
          "@AutoSlick.Iso[T, Repr](from, to) must be applied to a trait. Got: " + bad
        )
    }
  }

  def mixinMacro[T](annottees: Tree*): Tree = infoTap {
    val typName: TypeName = c.macroApplication match {
      case Apply(q"new $_.Mixin[${tArg: TypeName}]().macroTransform", _) =>
        tArg
      case other =>
        c.abort(
          c.enclosingPosition,
          "Unexpected macroApplication: " + other.toString
        )
    }

    annottees match {
      case List(
          q"..$mods trait $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        q"""
         $mods trait $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesGeneric(typName)}
           ..$objDefs
         }
         """
      case List(
          q"..$mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        q"""
         $mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesGeneric(typName)}
           ..$objDefs
         }
         """
      case bad =>
        c.abort(
          c.enclosingPosition,
          "@AutoSlick.Mixin[T] must be applied to a trait or object. Got: " + bad
        )
    }
  }

  def caseClassMacro(annottees: Tree*): Tree = infoTap {
    annottees match {
      // case class
      case List(clsDef: ClassDef) if clsDef.mods.hasFlag(Flag.CASE) =>
        q"""
         $clsDef
         object ${clsDef.name.toTermName} {
           ..${deriveInstancesGeneric(clsDef.name)}
         }
         """

      // case class with companion
      case List(
          clsDef: ClassDef,
          q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) if clsDef.mods.hasFlag(Flag.CASE) =>
        q"""
         $clsDef
         $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesGeneric(clsDef.name)}
           ..$objDefs
         }
       """

      case bad =>
        c.warning(c.enclosingPosition, s"match body: $bad")
        c.abort(
          c.enclosingPosition,
          "@AutoSlick must be used on a single-field case class or a trait extending SlickInstances[T]"
        )
    }
  }

  // Set to true for easy macro debugging
  private[this] val forceInfo = false

  private[this] val GR: TypeSymbol = typeOf[GetResult[_]].typeSymbol.asType
  private[this] val SP: TypeSymbol = typeOf[SetParameter[_]].typeSymbol.asType
  private[this] val BTT: TypeSymbol = typeOf[BaseTypedType[_]].typeSymbol.asType
  private[this] val TT: TypeSymbol = typeOf[TypedType[_]].typeSymbol.asType
  private[this] val JT: TypeSymbol = typeOf[JdbcType[_]].typeSymbol.asType
  private[this] val ISO: TypeSymbol =
    typeOf[Isomorphism[_, _]].typeSymbol.asType

  private[this] val DeriveClass: TypeSymbol =
    typeOf[Derive[_]].typeSymbol.asType
  private[this] val DeriveObj: TermSymbol =
    typeOf[Derive.type].termSymbol.asTerm

  private[this] def deriveInstancesIso(
      tpFrom: TypeName,
      tpTo: TypeName
  )(
      from: Tree,
      to: Tree
  ): List[Tree] =
    deriveInstances(
      tpFrom,
      Some(tpTo),
      q"$DeriveObj.iso[$tpFrom, $tpTo](from = $from, to = $to)"
    )

  private[this] def deriveInstancesGeneric(typName: TypeName): List[Tree] =
    deriveInstances(
      typName,
      None /* TODO there must be a way to figure this out */,
      q"new $DeriveObj.Partial[$typName].generic"
    )

  private[this] def deriveInstances(
      typName: TypeName,
      reprName: Option[TypeName],
      buildDerive: Tree
  ): List[Tree] = {

    def name(tc: String) = TermName(s"slick${tc}For${typName.toTermName}")

    val deriver = c.freshName(TermName("deriver"))
    val deriverType = reprName
      .map(r => tq"$DeriveObj.Aux[$typName, $r]")
      .getOrElse(tq"$DeriveClass[$typName]")

    List(
      // deriver has to be public because the `Repr` type is exposed publicly via the Isomorphism object
      // The name is synthetic though, so scala code won't be able to refer to it directly
      q"val $deriver: $deriverType = { import _root_.slick.jdbc.PostgresProfile.api._; $buildDerive }",
      q"final implicit lazy val ${name("GetResult")}: $GR[$typName] = $deriver.getResult",
      q"final implicit lazy val ${name("GetResultOpt")}: $GR[Option[$typName]] = $deriver.getResultOpt",
      q"final implicit lazy val ${name("SetParameter")}: $SP[$typName] = $deriver.setParameter",
      q"final implicit lazy val ${name("SetParameterOpt")}: $SP[Option[$typName]] = $deriver.setParameterOpt",
      q"final implicit lazy val ${name("ColumnType")}: $BTT[$typName] with $TT[$typName] with $JT[$typName] = $deriver.baseColumnType"
    ) ++
      reprName.map(
        r =>
          q"final implicit lazy val ${name("Isomorphism")}: $ISO[$typName, $r] = $deriver.isomorphism"
      )

  }

  private[this] def infoTap(tree: Tree): Tree = {
    c.info(c.enclosingPosition, tree.toString, forceInfo)
    tree
  }

}
