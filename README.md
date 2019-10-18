# mojoz

[![Build Status](https://travis-ci.org/guntiso/mojoz.svg?branch=develop)](https://travis-ci.org/guntiso/mojoz)

Manages relational database table metadata and queries.
Can generate SQL [DDL](https://en.wikipedia.org/wiki/Data_definition_language) statements.
Can generate Scala classes or [XSD](https://en.wikipedia.org/wiki/XML_Schema_(W3C)) to receive query results.
Used by [querease](https://github.com/guntiso/querease) to save and retrieve data.

## Table metadata

Table metadata can be loaded from YAML files
(see [YamlTableDefLoader](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/in/YamlTableDefLoader.html))
or from database using JDBC connection
(see [JdbcTableDefLoader](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/in/JdbcTableDefLoader$.html)).
Table metadata is described in [yaml 1.1](https://yaml.org/spec/1.1/), but yaml keys are further parsed by mojoz. For example, table definition

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

## [API docs](https://static.javadoc.io/org.mojoz/mojoz_2.13/1.1/mojoz/metadata/index.html)
