package tofu.optics.macros

import tofu.optics.PContains

import scala.reflect.macros.blackbox

class Optics(val prefix: String = "") extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro OpticsImpl.opticsAnnotationMacro
}

class POptics(val prefix: String = "") extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro OpticsImpl.popticsAnnotationMacro
}

class ClassyOptics(val prefix: String = "") extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro OpticsImpl.classyOpticsAnnotationMacro
}

class ClassyPOptics(val prefix: String = "") extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro OpticsImpl.classyPopticsAnnotationMacro
}

private[macros] class OpticsImpl(val c: blackbox.Context) {
  import c.universe._

  private lazy val PContainsC = typeOf[PContains[_, _, _, _]].typeConstructor.typeSymbol
  private lazy val promoteC   = typeOf[promote].typeSymbol

  def opticsAnnotationMacro(annottees: c.Expr[Any]*): c.Expr[Any] =
    annotationMacro(annottees, poly = false, classy = false)
  def classyOpticsAnnotationMacro(annottees: c.Expr[Any]*): c.Expr[Any] =
    annotationMacro(annottees, poly = false, classy = true)

  def popticsAnnotationMacro(annottees: c.Expr[Any]*): c.Expr[Any] =
    annotationMacro(annottees, poly = true, classy = false)
  def classyPopticsAnnotationMacro(annottees: c.Expr[Any]*): c.Expr[Any] =
    annotationMacro(annottees, poly = true, classy = true)

  def annotationMacro(annottees: Seq[c.Expr[Any]], poly: Boolean, classy: Boolean): c.Expr[Any] = {
    val LensesTpe = TypeName((poly, classy) match {
      case (false, false) => "Optics"
      case (false, true)  => "ClassyOptics"
      case (true, false)  => "POptics"
      case (true, true)   => "ClassyPOptics"
    })

    val prefix = c.macroApplication match {
      case Apply(Select(Apply(Select(New(Ident(LensesTpe)), t), args), _), _) if t == termNames.CONSTRUCTOR =>
        args match {
          case Literal(Constant(s: String)) :: Nil => s
          case _                                   => ""
        }
      case _ => ""
    }

    def labelType(s: String): Type = internal.constantType(Constant(s))

    def labelClass(p: ValDef, res: Tree)(s: Tree, t: Tree, a: Tree, b: Tree): (Tree, Tree) = {
      val fieldName     = p.name.toString
      def labelT        = labelType(fieldName)
      def classyT: Tree = tq"$PContainsC[$s, $t, $a, $b] with _root_.tofu.optics.Label[$labelT]"
      (q"$res.label[$labelT]", classyT)
    }

    def monolenses(tpname: TypeName, params: List[ValDef], classy: Boolean): List[Tree] = params.flatMap { param =>
      val lensName = TermName(prefix + param.name.decodedName)

      val res =
        q"""_root_.tofu.optics.macros.internal.Macro.mkContains[$tpname, $tpname, ${param.tpt}, ${param.tpt}](${param.name.toString})"""

      lazy val (resClassy, classyT) = labelClass(param, res)(tq"$tpname", tq"$tpname", param.tpt, param.tpt)

      if (classy)
        List(q"implicit val $lensName : $classyT  = $resClassy") ++
          promoteLens(param, lensName, tq"$tpname", param.tpt, Nil)
      else List(q"val $lensName = $res")
    }

    def optics(tpname: TypeName, tparams: List[TypeDef], params: List[ValDef], classy: Boolean): List[Tree] = {
      if (tparams.isEmpty) {
        monolenses(tpname, params, classy)
      } else {
        params.flatMap { param =>
          val lensName = TermName(prefix + param.name.decodedName)
          val q"x: $s" = q"x: $tpname[..${tparams.map(_.name)}]"
          val q"x: $a" = q"x: ${param.tpt}"

          val res = q"_root_.tofu.optics.macros.internal.Macro.mkContains[$s, $s, $a, $a](${param.name.toString})"

          lazy val (resClassy, classyT) = labelClass(param, res)(s, s, a, a)
          if (classy)
            List(q"implicit def $lensName[..$tparams] : $classyT = $resClassy") ++
              promoteLens(param, lensName, s, a, tparams)
          else List(q"""def $lensName[..$tparams] = $res""")
        }
      }
    }

    def poptics(tpname: TypeName, tparams: List[TypeDef], params: List[ValDef], classy: Boolean): List[Tree] = {
      if (tparams.isEmpty) {
        monolenses(tpname, params, classy)
      } else {
        // number of fields in which each tparam is used
        val tparamsUsages: Map[TypeName, Int] = params.foldLeft(tparams.map { _.name -> 0 }.toMap) { (acc, param) =>
          val typeNames = param.collect { case Ident(tn: TypeName) => tn }.toSet
          typeNames.foldLeft(acc) { (map, key) => map.get(key).fold(map) { value => map.updated(key, value + 1) } }
        }

        val groupedTpnames: Map[Int, Set[TypeName]] =
          tparamsUsages.toList.groupBy(_._2).map { case (n, tps) => (n, tps.map(_._1).toSet) }
        val phantomTpnames     = groupedTpnames.getOrElse(0, Set.empty)
        val singleFieldTpnames = groupedTpnames.getOrElse(1, Set.empty)

        params.flatMap { param =>
          val lensName        = TermName(prefix + param.name.decodedName)
          val tpnames         = param.collect { case Ident(tn: TypeName) => tn }.toSet
          val tpnamesToChange = tpnames.intersect(singleFieldTpnames) ++ phantomTpnames
          val tpnamesMap = tpnamesToChange.foldLeft((tparams.map(_.name).toSet ++ tpnames).map(x => (x, x)).toMap) {
            (acc, tpname) => acc.updated(tpname, c.freshName(tpname))
          }
          val defParams = tparams ++ tparams
            .filter(x => tpnamesToChange.contains(x.name))
            .map {
              case TypeDef(mods, name, tps, rhs) => TypeDef(mods, tpnamesMap(name), tps, rhs)
            }
            .toSet

          object tptTransformer extends Transformer {
            override def transform(tree: Tree): Tree = tree match {
              case Ident(tn: TypeName) => Ident(tpnamesMap(tn))
              case x                   => super.transform(x)
            }
          }

          val q"x: $s"                  = q"x: $tpname[..${tparams.map(_.name)}]"
          val q"x: $t"                  = q"x: $tpname[..${tparams.map(x => tpnamesMap(x.name))}]"
          val q"x: $a"                  = q"x: ${param.tpt}"
          val q"x: $b"                  = q"x: ${tptTransformer.transform(param.tpt)}"
          val res                       = q"_root_.tofu.optics.macros.internal.Macro.mkContains[$s, $t, $a, $b](${param.name.toString})"
          lazy val (resClassy, classyT) = labelClass(param, res)(s, t, a, b)

          if (classy) List(q"implicit def $lensName[..$defParams] : $classyT = $resClassy")
          else List(q"def $lensName[..$defParams] = $res")
        }
      }
    }

    val lensDefs = if (poly) poptics _ else optics _

    val result = annottees map (_.tree) match {
      case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
            :: Nil if mods.hasFlag(Flag.CASE) =>
        val name = tpname.toTermName
        q"""
         $classDef
         object $name {
           ..${lensDefs(tpname, tparams, paramss.head, classy)}
         }
         """
      case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
            :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
            :: Nil if mods.hasFlag(Flag.CASE) =>
        q"""
         $classDef
         $objMods object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ..${lensDefs(tpname, tparams, paramss.head, classy)}
           ..$objDefs
         }
         """
      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a case class")
    }

    c.Expr[Any](result)
  }

  private def promoteLens(valDef: ValDef, lens: TermName, s: Tree, a: Tree, tparams: List[TypeDef]): List[Tree] =
    if (valDef.mods.annotations.exists(isPromote)) {
      val name = TermName(c.freshName("promote" + valDef.name.decodedName.toString.capitalize))
      val t    = TypeName(c.freshName("t"))
      val tres = tq"$PContainsC[$s, $s, $t, $t]"
      val tpar = tq"$PContainsC[$a, $a, $t, $t]"
      List(q"""implicit def $name[$t, ..$tparams](implicit ev: $tpar): $tres = $lens >> ev""")
    } else Nil

  private def isPromote(tree: Tree) = c.typecheck(tree) match {
    case q"new $c" => c.symbol == promoteC
    case _         => false
  }

  private def debug(ss: Any*) = c.info(c.enclosingPosition, ss map {
    case null => "null"
    case s    => s.toString
  } mkString " ", true)
}
