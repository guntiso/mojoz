# mojoz

[![Build Status](https://travis-ci.org/guntiso/mojoz.svg?branch=develop)](https://travis-ci.org/guntiso/mojoz)

Manages relational database table metadata and queries.
Can generate SQL [DDL](https://en.wikipedia.org/wiki/Data_definition_language) statements.
Can generate Scala classes or [XSD](https://en.wikipedia.org/wiki/XML_Schema_(W3C)) to receive query results.
Used by [querease](https://github.com/guntiso/querease) to save and retrieve data.

## Table metadata

Table metadata is typically loaded from YAML resource files bundled with application
(see [YamlTableDefLoader](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/in/YamlTableDefLoader.html)).
Table metadata can also be loaded from database using JDBC connection
(see [JdbcTableDefLoader](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/in/JdbcTableDefLoader$.html)).

[Yaml 1.1](https://yaml.org/spec/1.1/) syntax is used for table metadata, but some keys and values are further parsed by mojoz.
Key names are:

* **table** - table name
* **comment**
* **columns** - collection of columns
* **pk** - primary key. If not provided, might be implied (based on columns) to be `id` or `code` or a pair of refs,
  as defined in [MdConventions](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/io/MdConventions.html).fromExternalPk
* **uk** - collection of unique keys
* **idx** - collection of indices
* **refs** - collection of customized or additional references to columns (foreign keys)
* any other (custom) keys can be used and are sent to `extras` field of [TableDef](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/TableDef.html)
  by YamlTableDefLoader.

For example, table definition
```yaml
table:   person
columns:
- id
- name                 ! 51
- surname                52
- mother.id              person.id
- father.id              person.id
```
corresponds to the following sql (as converted by [SqlWriter](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/out/SqlWriter$.html)):
```sql
create table person(
  id bigint,
  name varchar(51) not null,
  surname varchar(52),
  mother_id bigint,
  father_id bigint
);

alter table person add constraint pk_person primary key (id);

alter table person add constraint fk_person_mother_id foreign key (mother_id) references person(id);
alter table person add constraint fk_person_father_id foreign key (father_id) references person(id);
```
### Columns

Column information is mostly contained in yaml string or key except for comment which is \[first string\] value for the key.
General pattern for column information is:
```
name [nullability] [type] [length_or_total_digits] [fraction_digits] [(enum)] [= default] [: comment]
```
where
* _name_ is column name or for refs - _table_name.column_name_ or _alias.column_name_ (dot (.) is replaced with underscore(\_) to create column name)
* _nullability_ is optional exclamation mark (!) meaning **not** null
* _type_ is optional type name or for refs - _table_name.column_name_
* _length_or_total_digits_ is optional column length or total digits for number columns
* _fraction_digits_ is optional fraction digits for decimals
* _enum_ is optional list of comma and/or space separated values for the column (to enable spaces in values, all values should be wrapped in single quotes ('))
* _default_ is optional default value for the column
* _comment_ is optional text
* additionally, any (custom) keys can be used and are sent to `extras` field of [ColumnDef](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/ColumnDef.html)
  by YamlTableDefLoader.

## [API docs](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/index.html)
