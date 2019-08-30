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

  def macroTransform(annottees: Any*): Any = macro AutoSlickMacros.basicMacro
}

object AutoSlick {

  /** Generate shapeless.Generic-based instances instances for `T` in the annotated body.
    *
    * When applied to a case class, instances are attached to the companion object
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
    val (typName, reprName) = c.macroApplication match {
      case Apply(q"new $_[${tArg: Tree}]().macroTransform", _) =>
        val tArgC = c.typecheck(tArg, mode = c.TYPEmode)

        val tpe = tArgC.tpe
        val typeName = tpe.typeSymbol.name.toTypeName

        val fields = shapelessCopyPasta.fieldsOf(tpe)
        if (fields.length != 1)
          c.abort(c.enclosingPosition,
                  s"$typeName must have only one field, found: $fields")
        val reprType: Type = fields.head._2

        (typeName, reprType.typeSymbol.name.toTypeName)

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
           ..${deriveInstancesGeneric(typName, reprName)}
           ..$objDefs
         }
         """
      case List(
          q"..$mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        q"""
         $mods object $obj extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesGeneric(typName, reprName)}
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

  def basicMacro(annottees: Tree*): Tree = infoTap {
    annottees match {
      // case class
      case List(clsDef: ClassDef) if clsDef.mods.hasFlag(Flag.CASE) =>
        val reprType = wrappedField(clsDef)
        q"""
         $clsDef
         object ${clsDef.name.toTermName} {
           ..${deriveInstancesGeneric(clsDef.name, reprType)}
         }
         """

      // case class with companion
      case List(
          clsDef: ClassDef,
          q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) if clsDef.mods.hasFlag(Flag.CASE) =>
        val reprType = wrappedField(clsDef)
        q"""
         $clsDef
         $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ..${deriveInstancesGeneric(clsDef.name, reprType)}
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

  private[this] def wrappedField(clsDef: ClassDef): TypeName = {
    val q"$_ class ${tpname: TypeName}[..$_] $_(...$paramss) extends { ..$_ } with ..$_ { $_ => ..$_ }" =
      clsDef

    if (paramss.length != 1)
      c.abort(c.enclosingPosition,
              s"$tpname has more than 1 constructor field: $paramss")

    val List(q"$_ val $_: ${reprType: TypeName} = $_") = paramss.head
    reprType
  }

  // copy-paste from shapeless I can't reuse because they are defined inside a whitebox macro, and I have a blackbox macro
  private[this] object shapelessCopyPasta {
    def fieldsOf(tpe: Type): List[(TermName, Type)] = {
      val tSym = tpe.typeSymbol
      if (tSym.isClass && isAnonOrRefinement(tSym)) Nil
      else
        tpe.decls.sorted collect {
          case sym: TermSymbol if isCaseAccessorLike(sym) =>
            (sym.name.toTermName, sym.typeSignatureIn(tpe).finalResultType)
        }
    }
    def isAnonOrRefinement(sym: Symbol): Boolean = {
      val nameStr = sym.name.toString
      nameStr.contains("$anon") || nameStr == "<refinement>"
    }

    def isCaseAccessorLike(sym: TermSymbol): Boolean = {
      def isGetter =
        if (sym.owner.asClass.isCaseClass) sym.isCaseAccessor else sym.isGetter
      sym.isPublic && isGetter && !isNonGeneric(sym)
    }

    def isNonGeneric(sym: Symbol): Boolean = {
      import shapeless.nonGeneric
      def check(sym: Symbol): Boolean = {
        // See https://issues.scala-lang.org/browse/SI-7424
        sym.typeSignature // force loading method's signature
        sym.annotations.foreach(_.tree.tpe) // force loading all the annotations

        sym.annotations.exists(_.tree.tpe =:= typeOf[nonGeneric])
      }

      // See https://issues.scala-lang.org/browse/SI-7561
      check(sym) ||
      (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
      sym.overrides.exists(isNonGeneric)
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
      tpTo,
      q"$DeriveObj.iso[$tpFrom, $tpTo](from = $from, to = $to)"
    )

  private[this] def deriveInstancesGeneric(
      typName: TypeName,
      wrappedType: TypeName): List[Tree] = {

    val buildDerive = q"new $DeriveObj.Partial[$typName].generic"
    deriveInstances(
      typName,
      wrappedType,
      buildDerive
    )
  }

  private[this] def deriveInstances(
      typName: TypeName,
      reprName: TypeName,
      buildDerive: Tree
  ): List[Tree] = {

    def name(tc: String) = TermName(s"slick${tc}For${typName.toTermName}")

    // Use a fresh name so that we can mix in multiple to the same trait
    val deriver = c.freshName(TermName("deriver"))
    val deriverType = tq"$DeriveObj.Aux[$typName, $reprName]"

    List(
      q"private[this] val $deriver: $deriverType = { import _root_.slick.jdbc.PostgresProfile.api._; $buildDerive }",
      q"final implicit lazy val ${name("GetResult")}: $GR[$typName] = $deriver.getResult",
      q"final implicit lazy val ${name("GetResultOpt")}: $GR[Option[$typName]] = $deriver.getResultOpt",
      q"final implicit lazy val ${name("SetParameter")}: $SP[$typName] = $deriver.setParameter",
      q"final implicit lazy val ${name("SetParameterOpt")}: $SP[Option[$typName]] = $deriver.setParameterOpt",
      q"final implicit lazy val ${name("ColumnType")}: $BTT[$typName] with $TT[$typName] with $JT[$typName] = $deriver.baseColumnType",
      q"final implicit lazy val ${name("Isomorphism")}: $ISO[$typName, $reprName] = $deriver.isomorphism",
    )

  }

  private[this] def infoTap(tree: Tree): Tree = {
    // force=false doesn't actually prevent the compiler from printing, it just *allows* the compiler to not print.
    // When it does print, this gets noisy over a whole codebase, so don't bother calling c.info when false
    if (forceInfo) c.info(c.enclosingPosition, tree.toString, force = true)
    tree
  }

}
