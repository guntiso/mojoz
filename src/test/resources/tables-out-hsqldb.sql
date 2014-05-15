create table account(
  id bigint,
  bank_id bigint not null,
  billing_account varchar(64) not null,
  last_modified timestamp not null,
  constraint pk_account primary key (id)
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
  name_rus varchar(240),
  constraint pk_bank primary key (id)
);
comment on column bank.id is 'Ieraksta identifikators.';
comment on column bank.code is 'Bankas SWIFT kods.';
comment on column bank.country_code is 'Bankas valsts, izvēle no klasifikatora.';
comment on column bank.name is 'Bankas pilnais nosaukums.';
comment on column bank.name_eng is 'Bankas pilnais nosaukums, angliski.';
comment on column bank.name_rus is 'Bankas pilnais nosaukums, transliterēts krieviski.';

create table country(
  code varchar(2) not null,
  code3 varchar(3) not null,
  code_n3 varchar(3) not null,
  name varchar(64) not null,
  name_eng varchar(64),
  name_rus varchar(64),
  is_active boolean not null,
  is_eu boolean not null,
  constraint pk_country primary key (code)
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
  code varchar(3) not null,
  name varchar(100) not null,
  name_eng varchar(100) not null,
  name_rus varchar(100) not null,
  constraint pk_currency primary key (code)
);
comment on table currency is 'Sistēmā uzturēto valūtu klasifikators.';
comment on column currency.code is 'Starptautiski pieņemtais valūtas apzīmējums (burti).';
comment on column currency.name is 'Valūtas nosaukums.';
comment on column currency.name_eng is 'Valūtas nosaukums angliski.';
comment on column currency.name_rus is 'Valūtas nosaukums krieviski.';

create table test_table1(
  id bigint,
  code varchar(1) not null,
  col1 varchar(1),
  col2 varchar(1),
  col3 varchar(1),
  col4 varchar(1),
  col5 varchar(1),
  constraint pk_tt1_spec_id_code primary key (id, code)
);
create unique index uk_test_table1_code on test_table1(code);
create unique index uk_test_table1_code_col1 on test_table1(code, col1);
create unique index uk_test_table1_code_col2 on test_table1(code, col2);
create unique index uk_tt1_spec_col2 on test_table1(col2);
create unique index uk_tt2_spec_code_col2 on test_table1(code, col2);
create index idx_test_table1_id on test_table1(id);
create index idx_test_table1_id_col1 on test_table1(id, col1);
create index idx_tt1_spec_col3 on test_table1(col3);
create index idx_tt1_spec_col3_col4 on test_table1(col3, col4);
create index idx_tt1_spec_col3_col5a on test_table1(col3, col5);
create index idx_tt1_spec_col3_col5d on test_table1(col3, col5);

create table test_table2(
  id bigint,
  code varchar(1),
  name varchar(1) not null,
  constraint pk_test_table2 primary key (name)
);

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

alter table account add constraint fk_account_bank foreign key (bank_id) references bank(id);
alter table account_currency add constraint fk_account_currency_account foreign key (account_id) references account(id);
alter table account_currency add constraint fk_account_currency_currency foreign key (currency_code) references currency(code);
alter table bank add constraint fk_bank_country foreign key (country_code) references country(code);
