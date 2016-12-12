package examples

import scala.meta.{Mod, _}
import scala.meta.Mod.Final


class main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"def main(arg: Array[String]): Unit = { ..$stats }"
    q"object $name { $main }"
  }
}

class debug extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match  {
      case d @ q"..$mods def $name[..$tparams](...$paramss): $tpe = $expr" =>
        val body = q"""println("hello world")"""
        q"..$mods def ${name}[..$tparams](...$paramss): $tpe = $body"
    }
  }
}

class addFinal extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case something @ q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" =>
      val finalMod = collection.immutable.Seq(Final()) ++ mods
      q"..$finalMod class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template"
    }
  }
}

class cType extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any =  meta {
    defn match {
      case something @ q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" =>
        val argName = Term.Name(tname.value)
        val argType = Term.Param(Nil, Term.Name("i"), Some(Type.Name(tname.value)), None)
        val returnName = paramss.head.head
        val temp = Term.Name(returnName.name.value)
        val returnType = Type.Name(returnName.decltpe.get.syntax.filterNot(' '.equals(_)))
        val wrapFn = Term.Name("wrap" + argName.value)
        val unwrapFn = Term.Name("unwrap" + argName.value)
        val argType2 = Term.Param(Nil, Term.Name("i"), Some(returnType), None)
        val returnType2 = Type.Name(argName.value)
        val temp2 = Term.Apply(Term.Name(argName.value), Term.Name("i") :: Nil)
        Term.ApplyType
        val expr = q"""
              ..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template;
              ${Mod.Implicit()} def $unwrapFn[..$tparams]($argType): $returnType = i.$temp
              ${Mod.Implicit()} def $wrapFn[..$tparams]($argType2): $returnType2 = $temp2
           """
        println(expr)
        expr
    }
  }
}
