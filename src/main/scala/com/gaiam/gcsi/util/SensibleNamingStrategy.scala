package com.gaiam.gcsi.util

import org.hibernate.cfg.ImprovedNamingStrategy

/**
 *A hibernate naming strategy to turn SomeClass into a some_class table name and to join on some_class_id
 * @author knuttycombe
 */
class SensibleNamingStrategy extends ImprovedNamingStrategy {
  override def foreignKeyColumnName(propertyName: String, propertyEntityName: String, propertyTableName: String, referencedColumnName: String): String = {
    val s: String = super.foreignKeyColumnName(propertyName, propertyEntityName, propertyTableName, referencedColumnName)
    if (s.endsWith("_id")) s else s + "_id"
  }

}