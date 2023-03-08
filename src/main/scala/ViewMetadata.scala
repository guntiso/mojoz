package org.mojoz.metadata

import scala.collection.immutable.Map
import scala.collection.immutable.Seq

case class ViewDef_[+F](
  name: String,
  db: String,
  table: String,
  tableAlias: String,
  joins: Seq[String], // from clause
  filter: Seq[String], // where clause
  groupBy: Seq[String],
  having: Seq[String],
  orderBy: Seq[String],
  extends_ : String,
  comments: String,
  fields: Seq[F],
  saveTo: Seq[String],
  extras: Map[String, Any]) {
  private val nameToField: Map[String, F] =
    fields.map {
      case f: FieldDef_[_] => f.fieldName -> f.asInstanceOf[F]
      case f                  => (null, f)
    } .filter(_._1 != null)
      .toMap
  def field(fieldName: String): F = fieldOpt(fieldName).getOrElse(
    sys.error(s"Field $fieldName is not found in view $name")
  )
  def fieldOpt(fieldName: String): Option[F] = nameToField.get(fieldName)
}

case class FieldDef_[+T](
  table: String,
  tableAlias: String,
  name: String,
  alias: String,
  options: String, // persistence options
  isOverride: Boolean,
  isCollection: Boolean,
  isExpression: Boolean,
  expression: String,
  saveTo: String,
  resolver: String, // expression, calculates value to be saved
  nullable: Boolean,
  type_ : T,
  enum_ : Seq[String],
  joinToParent: String,
  orderBy: String,
  comments: String,
  extras: Map[String, Any]) {
  def this(name: String, type_ : T = null) = this(
    table = null,
    tableAlias = null,
    name = name,
    alias = null,
    options = null, // persistence options
    isOverride = false,
    isCollection = false,
    isExpression = false,
    expression = null,
    saveTo = null,
    resolver = null, // expression, calculates value to be saved
    nullable = true,
    type_  = type_,
    enum_ = null,
    joinToParent = null,
    orderBy = null,
    comments = null,
    extras = null
  )
  val fieldName = Option(alias).getOrElse(name)
}
