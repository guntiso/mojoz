# mojoz

[![Build Status](https://travis-ci.org/guntiso/mojoz.svg?branch=develop)](https://travis-ci.org/guntiso/mojoz)

Describes relational database table metadata and queries.
These descriptions are used by [querease](https://github.com/guntiso/querease) to save and retrieve data.

## Table metadata

Table metadata is described in [yaml 1.1](https://yaml.org/spec/1.1/), but yaml keys are further parsed by mojoz. For example:

```yaml
table:   person
columns:
- id
- name                 ! 51
- surname                52
- mother.id              person.id
- father.id              person.id
```
