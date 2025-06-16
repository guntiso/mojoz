create table table_i1(
  id bigint,
  code text,
  name text
);

alter table table_i1 add constraint pk_table_i1 primary key (id);
create index idx_table_i1_code_name on table_i1(code, name desc) where code is not null;
