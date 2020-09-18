package org.mojoz

import org.mojoz.metadata.io.IoColumnType

package object metadata {
  type MojozFieldDef = FieldDef[Type]
  type MojozViewDef = ViewDef[MojozFieldDef]

  type MojozFieldDefBase = FieldDef.FieldDefBase[Type]
  type MojozViewDefBase = ViewDef.ViewDefBase[MojozFieldDefBase]

  type MojozColumnDefBase = ColumnDef.ColumnDefBase[Type]
  type MojozTableDefBase = TableDef.TableDefBase[MojozColumnDefBase]

  type MojozColumnDef = ColumnDef[Type]
  type MojozTableDef = TableDef[MojozColumnDef]

  type IoColumnDef = ColumnDef[IoColumnType]
  type IoTableDef = TableDef[IoColumnDef]
}
