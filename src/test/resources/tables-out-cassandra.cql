create table cassandra_pk_test_table1(
  id bigint,
  name text,
  primary key (id)
);

create table cassandra_pk_test_table2(
  id bigint,
  name text,
  primary key (id)
);

create table cassandra_pk_test_table3(
  id bigint,
  name text,
  primary key (id, name)
);

create table cassandra_pk_test_table4(
  id bigint,
  name text,
  primary key (id, name)
);

create table cassandra_pk_test_table5(
  id bigint,
  name text,
  surname text,
  primary key ((id, name))
);

create table cassandra_pk_test_table6(
  id bigint,
  name text,
  surname text,
  primary key (id, name, surname)
) with clustering order by (name desc, surname desc)
   and comment = 'test 6 - with clustering order-by and comments';

create table cassandra_pk_test_table7(
  id bigint,
  name text,
  surname text,
  primary key (id, name, surname)
) with clustering order by (name desc, surname asc);

create table cassandra_pk_test_table8(
  id bigint,
  name text,
  surname text,
  primary key ((id, name), surname)
) with clustering order by (surname desc);

create table cassandra_pk_test_table9(
  id bigint,
  name text,
  surname text,
  primary key ((id, name, surname))
);

create table account(
  id bigint,
  bank_id bigint,
  billing_account text,
  last_modified timestamp,
  primary key (id)
) with comment = 'Klienta norēķina konts';
-- comment on column account.id is 'Ieraksta identifikators.';
-- comment on column account.bank_id is 'Bankas ID, sasaiste ar Bankas.';
-- comment on column account.billing_account is 'Norēķinu konts.';
-- comment on column account.last_modified is 'Pēdējo izmaiņu datums un laiks.';

create table account_currency(
  account_id bigint,
  currency_code text,
  primary key (account_id)
) with comment = 'Kontam pieejamās norēķinu valūtas - sistēmā konfigurētās valūtas pret kontu';
-- comment on column account_currency.account_id is 'Konta identifikators.';
-- comment on column account_currency.currency_code is 'Valūtas kods.';

create table bank(
  id bigint,
  code text,
  country_code text,
  name text,
  name_eng text,
  name_rus text,
  primary key (id)
);
-- comment on column bank.id is 'Ieraksta identifikators.';
-- comment on column bank.code is 'Bankas SWIFT kods.';
-- comment on column bank.country_code is 'Bankas valsts, izvēle no klasifikatora.';
-- comment on column bank.name is 'Bankas pilnais nosaukums.';
-- comment on column bank.name_eng is 'Bankas pilnais nosaukums, angliski.';
-- comment on column bank.name_rus is 'Bankas pilnais nosaukums, transliterēts krieviski.';

create table country(
  code text,
  code3 text,
  code_n3 text,
  name text,
  name_eng text,
  name_rus text,
  is_active boolean,
  is_eu boolean,
  primary key (code)
) with comment = 'Valstu klasifikators';
-- comment on column country.code is 'ISO 3166-1 divu burtu valsts kods';
-- comment on column country.code3 is 'ISO 3-burtu valsts kods';
-- comment on column country.code_n3 is 'ISO 3166-1 trīsciparu valsts kods';
-- comment on column country.name is 'Valsts nosaukums.';
-- comment on column country.name_eng is 'Valsts nosaukums angliski.';
-- comment on column country.name_rus is 'Valsts nosaukums krieviski.';
-- comment on column country.is_eu is 'Vai valsts ir Eiropas Savienības dalībvalsts';

create table currency(
  code text,
  name text,
  name_eng text,
  name_rus text,
  primary key (code)
) with comment = 'Sistēmā uzturēto valūtu klasifikators.';
-- comment on column currency.code is 'Starptautiski pieņemtais valūtas apzīmējums (burti).';
-- comment on column currency.name is 'Valūtas nosaukums.';
-- comment on column currency.name_eng is 'Valūtas nosaukums angliski.';
-- comment on column currency.name_rus is 'Valūtas nosaukums krieviski.';

create table person(
  id bigint,
  name text,
  surname text,
  mother_id bigint,
  father_id bigint,
  primary key (id)
);

create table test_table1(
  id bigint,
  code text,
  col1 text,
  col2 text,
  col3 text,
  col4 text,
  col5 text,
  longer_comments_col text,
  primary key (id, code)
) with comment = 'Test comment for table - to be escape''d';
-- comment on column test_table1.col2 is '';
-- comment on column test_table1.col3 is ' ';
-- comment on column test_table1.col4 is '  ';
-- comment on column test_table1.col5 is 'Test comment - to be escape''d';
-- comment on column test_table1.longer_comments_col is 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris rhoncus pharetra lorem, in pretium lacus interdum proin.';

create table test_table2(
  id bigint,
  code text,
  name text,
  description text,
  primary key (name)
) with comment = '';

create table test_table3(
  int_col int,
  int6_col int,
  integer36_col varint,
  long_col bigint,
  long16_col bigint,
  decimal266_col decimal,
  date_col date,
  datetime_col timestamp,
  string60_col text,
  string6k_col text,
  boolean_col boolean,
  bytes_col blob,
  primary key (int_col)
) with comment = ' ';

create table test_table4(
  int_col int,
  int6_col7 int,
  integer36_col8 varint,
  long_col bigint,
  long16_col7 bigint,
  decimal266_col8 decimal,
  string_col text,
  boolean_col_f boolean,
  boolean_col_t boolean,
  primary key (int_col)
) with comment = 'Multiline comments with
line 2
and line 3
';

create table test_table5(
  bank_id bigint,
  enum_for_int int,
  enum_for_long bigint,
  enum_for_decimal decimal,
  enum_with_diacritics text,
  enum_spesal_diacritics text,
  enum text,
  enum_ws text,
  primary key (bank_id)
) with comment = 'Alternative multiline comments
with line 2';

create table test_table6(
  id bigint,
  name text,
  primary key (id)
);

create table test_table7(
  key1 text,
  key2 text,
  primary key (key1, key2)
);

create table test_table8(
  id bigint,
  name text,
  primary key (id)
);

create table test_schema_1.test_table1(
  id bigint,
  name text,
  primary key (id)
);

create table test_schema_1.test_table2(
  t1_id bigint,
  primary key (t1_id)
);

create table test_schema_1.test_table3(
  t1_id bigint,
  primary key (t1_id)
);

create table test_schema_1.ts_table1(
  id bigint,
  name text,
  primary key (id)
);

create index uk_test_table1_code on test_table1(code);
-- create index uk_test_table1_code_col1 on test_table1(code, col1);
-- create index uk_test_table1_code_col2 on test_table1(code, col2 desc);
-- create index uk_test_table1_col1_col2_col3 on test_table1(col1, col2, col3);
create index uk_tt1_spec_col2 on test_table1(col2);
-- create index uk_tt2_spec_code_col2 on test_table1(code, col2);
-- create index idx_test_table1_id on test_table1(id);
-- create index idx_test_table1_id_col1 on test_table1(id, col1);
create index idx_tt1_spec_col3 on test_table1(col3);
-- create index idx_tt1_spec_col3_col4 on test_table1(col3, col4);
-- create index idx_tt1_spec_col3_col5a on test_table1(col3, col5);
-- create index idx_tt1_spec_col3_col5d on test_table1(col3, col5 desc);

create index uk_test_table1_id on test_schema_1.test_table1(id);

-- alter table account add constraint fk_account_bank_id foreign key (bank_id) references bank(id);
-- alter table account_currency add constraint fk_account_currency_account_id foreign key (account_id) references account(id);
-- alter table account_currency add constraint fk_account_currency_currency_code foreign key (currency_code) references currency(code);
-- alter table bank add constraint fk_bank_country_code foreign key (country_code) references country(code);
-- alter table person add constraint fk_person_mother_id foreign key (mother_id) references person(id);
-- alter table person add constraint fk_person_father_id foreign key (father_id) references person(id);
-- alter table test_table2 add constraint fk_test_table2_code_name foreign key (code, name) references test_table1(code, col1) on delete cascade;
-- alter table test_table2 add constraint fk_tt2_spec_code_name foreign key (code, name) references test_table1(code, col2);
-- alter table test_table5 add constraint fk_test_table5_bank_id foreign key (bank_id) references bank(id) on delete cascade;
-- alter table test_schema_1.test_table2 add constraint fk_test_table2_t1_id foreign key (t1_id) references test_schema_1.test_table1(id);
-- alter table test_schema_1.test_table3 add constraint fk_test_table3_t1_id foreign key (t1_id) references test_schema_1.ts_table1(id) on delete cascade;
