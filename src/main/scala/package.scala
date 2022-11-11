package org.mojoz

import org.mojoz.metadata.io.IoColumnType
import org.mojoz.metadata.in.JdbcTableDefLoader.JdbcColumnType
import org.mojoz.metadata.TableMetadata.{CheckConstraint, DbIndex, Ref}

import scala.collection.immutable.Map
import scala.collection.immutable.Seq

package object metadata {
  type FieldDef       = FieldDef_[Type]
  type ViewDef        = ViewDef_[FieldDef]
  type ColumnDef      = ColumnDef_[Type]
  type TableDef       = TableDef_[ColumnDef]

  type IoFieldDef     = FieldDef_[IoColumnType]
  type IoViewDef      = ViewDef_[IoFieldDef]
  type IoColumnDef    = ColumnDef_[IoColumnType]
  type IoTableDef     = TableDef_[IoColumnDef]

  type JdbcColumnDef  = ColumnDef_[JdbcColumnType]
  type JdbcTableDef   = TableDef_[JdbcColumnDef]

  def  FieldDef(
    table:        String,
    tableAlias:   String,
    name:         String,
    alias:        String,
    options:      String, // persistence options
    isOverride:   Boolean,
    isCollection: Boolean,
    isExpression: Boolean,
    expression:   String,
    saveTo:       String,
    resolver:     String, // expression, calculates value to be saved
    nullable:     Boolean,
    type_ :       Type,
    enum_ :       Seq[String],
    joinToParent: String,
    orderBy:      String,
    comments:     String,
    extras:       Map[String, Any],
  ) = FieldDef_(
    table, tableAlias, name, alias, options, isOverride, isCollection,
    isExpression, expression, saveTo, resolver, nullable, type_, enum_,
    joinToParent, orderBy, comments, extras,
  )

  def  ViewDef(
    name:         String,
    db:           String,
    table:        String,
    tableAlias:   String,
    joins:        Seq[String], // from clause
    filter:       Seq[String], // where clause
    groupBy:      Seq[String],
    having:       Seq[String],
    orderBy:      Seq[String],
    extends_ :    String,
    comments:     String,
    fields:       Seq[FieldDef],
    saveTo:       Seq[String],
    extras:       Map[String, Any],
  ) = ViewDef_(
    name, db, table, tableAlias, joins, filter, groupBy, having,
    orderBy, extends_, comments, fields, saveTo, extras,
  )

  def  ColumnDef(
    name:         String,
    type_ :       Type,
    nullable:     Boolean,
    dbDefault:    String,
    enum_ :       Seq[String],
    comments:     String,
    extras: Map[String, Any],
  ) = ColumnDef_(name, type_, nullable, dbDefault, enum_, comments, extras)

  def  TableDef(
    db:           String,
    name:         String,
    comments:     String,
    cols:         Seq[ColumnDef],
    pk:           Option[DbIndex],
    uk:           Seq[DbIndex],
    ck:           Seq[CheckConstraint],
    idx:          Seq[DbIndex],
    refs:         Seq[Ref],
    extras:       Map[String, Any],
  ) = TableDef_(db, name, comments, cols, pk, uk, ck, idx, refs, extras)

  def  IoColumnDef(
    name:         String,
    type_ :       IoColumnType,
    nullable:     Boolean,
    dbDefault:    String,
    enum_ :       Seq[String],
    comments:     String,
    extras: Map[String, Any],
  ) = ColumnDef_(name, type_, nullable, dbDefault, enum_, comments, extras)

  def  IoTableDef(
    db:           String,
    name:         String,
    comments:     String,
    cols:         Seq[IoColumnDef],
    pk:           Option[DbIndex],
    uk:           Seq[DbIndex],
    ck:           Seq[CheckConstraint],
    idx:          Seq[DbIndex],
    refs:         Seq[Ref],
    extras:       Map[String, Any],
  ) = TableDef_(db, name, comments, cols, pk, uk, ck, idx, refs, extras)

  def  JdbcColumnDef(
    name:         String,
    type_ :       JdbcColumnType,
    nullable:     Boolean,
    dbDefault:    String,
    enum_ :       Seq[String],
    comments:     String,
    extras:       Map[String, Any],
  ) = ColumnDef_(name, type_, nullable, dbDefault, enum_, comments, extras)

  def  JdbcTableDef(
    db:           String,
    name:         String,
    comments:     String,
    cols:         Seq[JdbcColumnDef],
    pk:           Option[DbIndex],
    uk:           Seq[DbIndex],
    ck:           Seq[CheckConstraint],
    idx:          Seq[DbIndex],
    refs:         Seq[Ref],
    extras:       Map[String, Any],
  ) = TableDef_(db, name, comments, cols, pk, uk, ck, idx, refs, extras)
}
