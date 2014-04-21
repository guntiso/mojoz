create table account(
  id numeric(18),
  bank_id numeric(18) not null,
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
  account_id numeric(18) not null,
  currency_code varchar(3) not null
);
comment on table account_currency is 'Kontam pieejamās norēķinu valūtas - sistēmā konfigurētās valūtas pret kontu';
comment on column account_currency.account_id is 'Konta identifikators.';
comment on column account_currency.currency_code is 'Valūtas kods.';

create table bank(
  id numeric(18),
  code varchar(16) not null,
  country_code varchar(2),
  name varchar(240) not null,
  name_eng varchar(240),
  name_rus varchar(240),
  constraint pk_bank primary key (id)
);
comment on table bank is '';
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
comment on column country.is_active is '';
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

alter table account add constraint fk_account_bank foreign key (bank_id) references bank(id);
alter table account_currency add constraint fk_account_currency_account foreign key (account_id) references account(id);
alter table account_currency add constraint fk_account_currency_currency foreign key (currency_code) references currency(code);
alter table bank add constraint fk_bank_country foreign key (country_code) references country(code);
