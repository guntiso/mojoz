package org.mojoz.metadata

import scala.collection.immutable.Map
import scala.collection.immutable.Seq

import ViewDef._
import FieldDef._

object ViewDef {
  trait ViewDefBase[+F] {
    val name: String
    val db: String
    val table: String
    val tableAlias: String
    val joins: Seq[String] // from clause
    val filter: Seq[String] // where clause
    val groupBy: Seq[String]
    val having: Seq[String]
    val orderBy: Seq[String]
    val extends_ : String
    val comments: String
    val fields: Seq[F]
    val saveTo: Seq[String]
    def field(fieldName: String): F = null.asInstanceOf[F]
    def fieldOpt(fieldName: String): Option[F] = None
  }
}
case class ViewDef[+F](
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
  extras: Map[String, Any]) extends ViewDefBase[F] {
  private val nameToField: Map[String, F] =
    fields.map {
      case f: FieldDefBase[_] => Option(f.alias).getOrElse(f.name) -> f.asInstanceOf[F]
      case f                  => (null, f)
    } .filter(_._1 != null)
      .toMap
  override def field(fieldName: String): F = fieldOpt(fieldName).getOrElse(
    sys.error(s"Field $fieldName is not found in view $name")
  )
  override def fieldOpt(fieldName: String): Option[F] = nameToField.get(fieldName)
}

object FieldDef {
  trait FieldDefBase[+T] {
    val table: String
    val tableAlias: String
    val name: String
    val alias: String
    val options: String // persistence options
    val isOverride: Boolean
    val isCollection: Boolean
    val isExpression: Boolean
    val expression: String
    val saveTo: String
    val resolver: String // expression, calculates value to be saved
    val nullable: Boolean
    val type_ : T
    val enum_ : Seq[String]
    val joinToParent: String
    val orderBy: String
    val comments: String
  }
}
case class FieldDef[+T](
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
  extras: Map[String, Any]) extends FieldDefBase[T] {
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
  def this(that: FieldDefBase[T]) = this(
    table = that.table,
    tableAlias = that.tableAlias,
    name = that.name,
    alias = that.alias,
    options = that.options, // persistence options
    isOverride = that.isOverride,
    isCollection = that.isCollection,
    isExpression = that.isExpression,
    expression = that.expression,
    saveTo = that.saveTo,
    resolver = that.resolver, // expression, calculates value to be saved
    nullable = that.nullable,
    type_ = that.type_,
    enum_ = that.enum_,
    joinToParent = that.joinToParent,
    orderBy = that.orderBy,
    comments = that.comments,
    extras = that match { case f: FieldDef[_] => f.extras case _ => null }
  )
}
