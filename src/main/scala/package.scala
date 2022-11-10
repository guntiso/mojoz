package org.mojoz

import org.mojoz.metadata.io.IoColumnType
import org.mojoz.metadata.in.JdbcTableDefLoader.JdbcColumnType

package object metadata {
  type MojozFieldDef  = FieldDef[Type]
  type MojozViewDef   = ViewDef[MojozFieldDef]
  type MojozColumnDef = ColumnDef[Type]
  type MojozTableDef  = TableDef[MojozColumnDef]

  type IoFieldDef     = FieldDef[IoColumnType]
  type IoViewDef      = ViewDef[IoFieldDef]
  type IoColumnDef    = ColumnDef[IoColumnType]
  type IoTableDef     = TableDef[IoColumnDef]

  type JdbcColumnDef  = ColumnDef[JdbcColumnType]
  type JdbcTableDef   = TableDef[JdbcColumnDef]
}
