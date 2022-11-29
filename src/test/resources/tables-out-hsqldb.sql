create table account(
  id bigint,
  bank_id bigint not null,
  billing_account varchar(64) not null,
  last_modified timestamp not null
);
comment on table account is 'Klienta norēķina konts';
comment on column account.id is 'Ieraksta identifikators.';
comment on column account.bank_id is 'Bankas ID, sasaiste ar Bankas.';
comment on column account.billing_account is 'Norēķinu konts.';
comment on column account.last_modified is 'Pēdējo izmaiņu datums un laiks.';

create table account_currency(
  account_id bigint not null,
  currency_code varchar(3) not null
);
comment on table account_currency is 'Kontam pieejamās norēķinu valūtas - sistēmā konfigurētās valūtas pret kontu';
comment on column account_currency.account_id is 'Konta identifikators.';
comment on column account_currency.currency_code is 'Valūtas kods.';

create table bank(
  id bigint,
  code varchar(16) not null,
  country_code varchar(2),
  name varchar(240) not null,
  name_eng varchar(240),
  name_rus varchar(240)
);
comment on column bank.id is 'Ieraksta identifikators.';
comment on column bank.code is 'Bankas SWIFT kods.';
comment on column bank.country_code is 'Bankas valsts, izvēle no klasifikatora.';
comment on column bank.name is 'Bankas pilnais nosaukums.';
comment on column bank.name_eng is 'Bankas pilnais nosaukums, angliski.';
comment on column bank.name_rus is 'Bankas pilnais nosaukums, transliterēts krieviski.';

create table country(
  code varchar(2) check (code in ('LV', 'TO', 'LT')),
  code3 varchar(3) not null,
  code_n3 varchar(3) not null,
  name varchar(64) not null,
  name_eng varchar(64),
  name_rus varchar(64),
  is_active boolean not null,
  is_eu boolean not null
);
comment on table country is 'Valstu klasifikators';
comment on column country.code is 'ISO 3166-1 divu burtu valsts kods';
comment on column country.code3 is 'ISO 3-burtu valsts kods';
comment on column country.code_n3 is 'ISO 3166-1 trīsciparu valsts kods';
comment on column country.name is 'Valsts nosaukums.';
comment on column country.name_eng is 'Valsts nosaukums angliski.';
comment on column country.name_rus is 'Valsts nosaukums krieviski.';
comment on column country.is_eu is 'Vai valsts ir Eiropas Savienības dalībvalsts';

create table currency(
  code varchar(3) check (code in ('USD', 'EUR')),
  name varchar(100) not null,
  name_eng varchar(100) not null,
  name_rus varchar(100) not null
);
comment on table currency is 'Sistēmā uzturēto valūtu klasifikators.';
comment on column currency.code is 'Starptautiski pieņemtais valūtas apzīmējums (burti).';
comment on column currency.name is 'Valūtas nosaukums.';
comment on column currency.name_eng is 'Valūtas nosaukums angliski.';
comment on column currency.name_rus is 'Valūtas nosaukums krieviski.';

create table person(
  id bigint,
  name varchar(51) not null,
  surname varchar(52),
  mother_id bigint,
  father_id bigint
);

create table test_table1(
  id bigint,
  code varchar(1),
  col1 varchar(1),
  col2 varchar(1),
  col3 varchar(1),
  col4 varchar(1),
  col5 varchar(1),
  longer_comments_col varchar(1)
);
comment on table test_table1 is 'Test comment for table - to be escape''d';
comment on column test_table1.col2 is '';
comment on column test_table1.col3 is ' ';
comment on column test_table1.col4 is '  ';
comment on column test_table1.col5 is 'Test comment - to be escape''d';
comment on column test_table1.longer_comments_col is 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris rhoncus pharetra lorem, in pretium lacus interdum proin.';

create table test_table2(
  id bigint,
  code varchar(1),
  name varchar(1),
  description clob
);
comment on table test_table2 is '';

create table test_table3(
  int_col integer,
  int6_col numeric(6),
  integer36_col numeric(36),
  long_col bigint,
  long16_col numeric(16),
  decimal266_col numeric(26, 6),
  date_col date,
  datetime_col timestamp,
  string60_col varchar(60),
  string6k_col varchar(6000),
  boolean_col boolean,
  bytes_col blob
);
comment on table test_table3 is ' ';

create table test_table4(
  int_col integer default 6,
  int6_col7 numeric(6) default 7,
  integer36_col8 numeric(36) default 8,
  long_col bigint default 6,
  long16_col7 numeric(16) default 7,
  decimal266_col8 numeric(26, 6) default 8.000000,
  string_col varchar(60) default 'default value',
  boolean_col_f boolean default false,
  boolean_col_t boolean default true
);
comment on table test_table4 is 'Multiline comments with
line 2
and line 3
';

create table test_table5(
  bank_id bigint,
  enum_for_int integer check (enum_for_int in (-1, 0, 1)),
  enum_for_long bigint check (enum_for_long in (-99, 0, 99)),
  enum_for_decimal numeric(3, 1) check (enum_for_decimal in (-9.9, 0, 9.9)),
  enum_with_diacritics varchar(6) check (enum_with_diacritics in ('rūķīši', 'darbā', 'vai', 'mājās')),
  enum_spešal_diacritics varchar(6) check (enum_spešal_diacritics in ('bladāc')),
  enum varchar(5) check (enum in ('list1', 'list2')),
  enum_ws varchar(6) check (enum_ws in ('list 1', 'list 2'))
);
comment on table test_table5 is 'Alternative multiline comments
with line 2';

create table test_table6(
  id bigint,
  name varchar(50)
);

create table test_table7(
  key1 varchar(20),
  key2 varchar(20)
);

create table test_table8(
  id bigint,
  name varchar(50)
);

create table test_schema_1.test_table1(
  id bigint not null,
  name varchar(50)
);

create table test_schema_1.test_table2(
  t1_id bigint
);

create table test_schema_1.test_table3(
  t1_id bigint
);

create table test_schema_1.ts_table1(
  id bigint,
  name varchar(50)
);

create table person(
  id bigint,
  full_name varchar(128) not null
);

alter table account add constraint pk_account primary key (id);

alter table bank add constraint pk_bank primary key (id);

alter table country add constraint pk_country primary key (code);

alter table currency add constraint pk_currency primary key (code);

alter table person add constraint pk_person primary key (id);

alter table test_table1 add constraint pk_tt1_spec_id_code primary key (id, code);
alter table test_table1 add constraint uk_test_table1_code unique(code);
alter table test_table1 add constraint uk_test_table1_code_col1 unique(code, col1);
create unique index uk_test_table1_code_col2 on test_table1(code, col2);
alter table test_table1 add constraint uk_test_table1_col1_col2_col3 unique(col1, col2, col3);
alter table test_table1 add constraint uk_tt1_spec_col2 unique(col2);
alter table test_table1 add constraint uk_tt2_spec_code_col2 unique(code, col2);
create index idx_test_table1_id on test_table1(id);
create index idx_test_table1_id_col1 on test_table1(id, col1);
create index idx_tt1_spec_col3 on test_table1(col3);
create index idx_tt1_spec_col3_col4 on test_table1(col3, col4);
create index idx_tt1_spec_col3_col5a on test_table1(col3, col5);
create index idx_tt1_spec_col3_col5d on test_table1(col3, col5);

alter table test_table2 add constraint pk_test_table2 primary key (name);

alter table test_table6 add constraint pk_test_table6 primary key (id);

alter table test_table7 add constraint pk_test_table7 primary key (key1, key2);

alter table test_schema_1.test_table1 add constraint uk_test_table1_id unique(id);

alter table test_schema_1.ts_table1 add constraint pk_ts_table1 primary key (id);

alter table person add constraint pk_person primary key (id);

alter table account add constraint fk_account_bank_id foreign key (bank_id) references bank(id);
alter table account_currency add constraint fk_account_currency_account_id foreign key (account_id) references account(id);
alter table account_currency add constraint fk_account_currency_currency_code foreign key (currency_code) references currency(code);
alter table bank add constraint fk_bank_country_code foreign key (country_code) references country(code);
alter table person add constraint fk_person_mother_id foreign key (mother_id) references person(id);
alter table person add constraint fk_person_father_id foreign key (father_id) references person(id);
alter table test_table2 add constraint fk_test_table2_code_name foreign key (code, name) references test_table1(code, col1) on delete cascade;
alter table test_table2 add constraint fk_tt2_spec_code_name foreign key (code, name) references test_table1(code, col2);
alter table test_table5 add constraint fk_test_table5_bank_id foreign key (bank_id) references bank(id) on delete cascade;
alter table test_schema_1.test_table2 add constraint fk_test_table2_t1_id foreign key (t1_id) references test_schema_1.test_table1(id);
alter table test_schema_1.test_table3 add constraint fk_test_table3_t1_id foreign key (t1_id) references test_schema_1.ts_table1(id) on delete cascade;
