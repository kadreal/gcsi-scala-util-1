package com.gaiam.gcsi.util

import scala.language.experimental.macros

object Macros {
  import scala.reflect.macros.Context

  def build_enum_map[a : c.WeakTypeTag, t : c.WeakTypeTag](c: Context)() = {
    import c.universe._


    val mapApply = Select(reify(Map).tree, newTermName("apply"))

    val pairs = weakTypeOf[t].declarations.collect{
      case m : ModuleSymbol => { //Todo add checking so we don't add non-enum objects
        val name = c.literal(m.name.decoded)
        val value = c.Expr[a](Select(c.prefix.tree, m.name))
        reify[Tuple2[String, a]](name.splice -> value.splice).tree
      }
    }
    c.Expr[Map[String, a]](Apply(mapApply, pairs.toList))
  }
}