package some.caseclass.pack

case class Account (
  id: java.lang.Long = null,
  bankId: java.lang.Long = null,
  billingAccount: String = null,
  currency: List[String] = Nil
)
case class AccountDetails (
  bankCode: String = null,
  bankName: String = null
)
case class BankListRow (
  id: java.lang.Long = null,
  code: String = null,
  countryCode: String = null,
  countryName: String = null,
  name: String = null
)
case class NamedInlineView (
  childId: java.lang.Long = null,
  childName: String = null
)
case class NamedInlineView2 (
  childName: String = null
)
case class NamedInlineView3 (
  childName: String = null
)
case class NoFields (
)
case class NoFieldsB (
)
case class NoFieldsExtendedB (
  id: java.lang.Long = null
)
case class NoFieldsExtendedWithFields (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null,
  countryName: String = null
)
case class NoTable (
  someId: java.lang.Long = null,
  someName: String = null,
  someDate: java.sql.Date = null,
  someDateTime: java.sql.Timestamp = null,
  isBoolean: java.lang.Boolean = null,
  isBooleanExpr: java.lang.Boolean = null,
  isNotBoolean: java.sql.Date = null
)
case class OneFielder (
  theOne: String = null
)
case class Person (
  name: String = null,
  surname: String = null,
  motherName: String = null,
  fatherName: String = null,
  maternalGrandmotherName: String = null,
  maternalGrandfatherName: String = null,
  maternalGrandfatherSurname: String = null,
  fatherMotherName: String = null,
  fatherFatherName: String = null,
  fatherFatherFatherSurname: String = null
)
case class ResolverTest1 (
  id: java.lang.Long = null,
  mother: String = null,
  father: String = null
)
case class ResolverTest2 (
  account: String = null,
  currencyName: String = null
)
case class ResolverTest3 (
  id: java.lang.Long = null,
  code: String = null
)
case class ResolverTest4 (
  id: java.lang.Long = null,
  name: String = null
)
case class ResolverTest5 (
  id: java.lang.Long = null,
  name: String = null
)
case class ResolverTest6 (
  id: java.lang.Long = null,
  name: String = null
)
case class ResolverTest7 (
  id: java.lang.Long = null,
  name: String = null,
  country: String = null
)
case class ResolverTest7b (
  id: java.lang.Long = null,
  country: String = null
)
case class ResolverTest8 (
  id: java.lang.Long = null,
  bank: String = null
)
case class ResolverTest8b (
  id: java.lang.Long = null,
  bankId: java.lang.Long = null
)
case class ResolverTest9 (
  id: java.lang.Long = null,
  mother: String = null,
  father: String = null
)
case class TypeOverrideTest (
  bank: java.lang.Long = null,
  lastModified: java.sql.Date = null
)
case class WithAnonymousInlineView (
  someId: java.lang.Long = null,
  someName: String = null,
  someChildren: List[WithAnonymousInlineViewSomeChildren] = Nil
)
case class WithAnonymousInlineViewSomeChildren (
  childId: java.lang.Long = null,
  childName: String = null
)
case class WithEmptyChildSaveTo (
  singleChild: WithEmptyChildSaveToSingleChild = null
)
case class WithEmptyChildSaveToSingleChild (
  childName: String = null
)
case class WithNamedInlineView (
  someId: java.lang.Long = null,
  someName: String = null,
  someChildren: List[NamedInlineView] = Nil
)
case class WithNamedInlineView2 (
  someChildren: List[NamedInlineView2] = Nil
)
case class WithNamedInlineView3 (
  singleChild: NamedInlineView3 = null
)
case class WithScalaKeywords (
  `this`: String = null,
  `super`: String = null,
  `new`: String = null,
  `true`: String = null,
  `lazy`: String = null,
  `null`: String = null,
  `case`: String = null,
  `final`: String = null,
  `type`: String = null
)
// end
