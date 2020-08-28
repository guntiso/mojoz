package org.mojoz.metadata

import scala.collection.immutable.Map
import scala.collection.immutable.Seq

import ViewDef._
import FieldDef._

object ViewDef {
  trait ViewDefBase[+F] {
    val name: String
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
  }
}
case class ViewDef[+F](
  name: String,
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
  extras: Map[String, Any]) extends ViewDefBase[F]

object FieldDef {
  trait FieldDefBase[+T] {
    val table: String
    val tableAlias: String
    val name: String
    val alias: String
    val options: String // persistence options
    val isCollection: Boolean
    val isExpression: Boolean
    val expression: String
    val saveTo: String
    val resolver: String // expression, calculates value to be saved
    val nullable: Boolean
    val type_ : T
    val enum: Seq[String]
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
  isCollection: Boolean,
  isExpression: Boolean,
  expression: String,
  saveTo: String,
  resolver: String, // expression, calculates value to be saved
  nullable: Boolean,
  type_ : T,
  enum: Seq[String],
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
    isCollection = false,
    isExpression = false,
    expression = null,
    saveTo = null,
    resolver = null, // expression, calculates value to be saved
    nullable = true,
    type_  = type_,
    enum = null,
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
    isCollection = that.isCollection,
    isExpression = that.isExpression,
    expression = that.expression,
    saveTo = that.saveTo,
    resolver = that.resolver, // expression, calculates value to be saved
    nullable = that.nullable,
    type_ = that.type_,
    enum = that.enum,
    joinToParent = that.joinToParent,
    orderBy = that.orderBy,
    comments = that.comments,
    extras = that match { case f: FieldDef[_] => f.extras case _ => null }
  )
}
