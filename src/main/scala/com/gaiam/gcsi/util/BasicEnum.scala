package com.gaiam.gcsi.util

import java.io.Serializable
import java.sql.{PreparedStatement, ResultSet, Types}

import org.hibernate.usertype.UserType

import scala.language.experimental.macros
import scala.util.Try


//Todo make tests for this and the hibernator
trait BasicEnum[A <: AnyRef] {

  protected val items : Map[String,A]
  def buildMap[A, T]() : Map[String, A] = macro Macros.build_enum_map[A, T]
  private val clazzName = this.getClass.getName.dropRight(1)
  def withName(value : String) : A = items.get(value).getOrElse( {
    throw new NoSuchElementException("no such element: " + value)
  })

}

/**
 * Used to map a scala enum into a string field in the database for hibernate.
 */
abstract class BaseEnumHibernator extends UserType {

  val enum : BasicEnum[(_ <: AnyRef)]
  override def sqlTypes(): Array[Int] = Array(Types.VARCHAR)

  override def deepCopy(value: AnyRef): AnyRef = value

  override def nullSafeGet(rs: ResultSet, names: Array[String], owner: AnyRef): AnyRef = {
    val s = rs.getString(names(0))
    if(s != null){
      enum.withName(s)
    } else {
      null
    }
  }

  override def replace(original: AnyRef, target: AnyRef, owner: AnyRef): AnyRef = target

  override def isMutable: Boolean = false

  override def assemble(cached: Serializable, owner: AnyRef): AnyRef = enum.withName(cached.toString)

  override def nullSafeSet(st: PreparedStatement, value: AnyRef, index: Int): Unit = {
    if (value == null){
      st.setNull(index, sqlTypes()(0))
    } else {
      st.setString(index, value.toString)
    }
  }

  override def hashCode(x: AnyRef): Int = x.hashCode()

  override def disassemble(value: AnyRef): Serializable = value.toString

  override def equals(x: AnyRef, y: AnyRef): Boolean = x == y
}