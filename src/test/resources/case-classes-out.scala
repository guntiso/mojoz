package some.caseclass.pack

case class Account (
  id: java.lang.Long = null,
  bankId: java.lang.Long = null,
  billingAccount: String = null,
  currency: List[String] = Nil
)
case class AccountDetails /* extends Account */ (
  // --- Account
  id: java.lang.Long = null,
  bankId: java.lang.Long = null,
  // billingAccount: String = null
  currency: List[String] = Nil,
  // --- AccountDetails
  bankCode: String = null,
  bankName: String = null,
  /* override */ billingAccount: String = null
)
case class BankListRow (
  id: java.lang.Long = null,
  code: String = null,
  countryCode: String = null,
  countryName: String = null,
  name: String = null
)
case class CardinalityOverideTest (
  name: String = null,
  surname: String = null
)
case class ExpressionTest1 (
  id: java.lang.Long = null,
  bank: String = null
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
case class NoFieldsExtendedB /* extends NoFieldsB */ (
  // --- NoFieldsB
  // --- NoFieldsExtendedB
  id: java.lang.Long = null
)
case class NoFieldsExtendedWithFields /* extends NoFields */ (
  // --- NoFields
  // --- NoFieldsExtendedWithFields
  id: java.lang.Long = null,
  code: String = null,
  name: String = null,
  countryName: String = null
)
case class NoTable (
  someId: java.lang.Long = null,
  someName: String = null,
  someDate: java.time.LocalDate = null,
  someDateTime: java.time.LocalDateTime = null,
  isBoolean: java.lang.Boolean = null,
  isBooleanExpr: java.lang.Boolean = null,
  isNotBoolean: java.time.LocalDate = null
)
case class OneFielder (
  theOne: String = null
)
case class OtherDbPersonView (
  id: java.lang.Long = null,
  fullName: String = null
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
case class ReferencedColumnTypesTest (
  field1: String = null,
  field2: java.lang.Integer = null
)
case class ResolverTest1 (
  id: java.lang.Long = null,
  mother: String = null,
  father: String = null
)
case class ResolverTest2 (
  account: String = null,
  currencyName: String = null,
  accountBankCode: String = null,
  accountBankNameEng: String = null
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
case class SchemaTest1 (
  t1Id: java.lang.Long = null,
  t1Name: String = null
)
case class SchemaTest2 (
  id: java.lang.Long = null,
  testSchema1TestTable1Name: String = null
)
case class SchemaTest3 (
  id: java.lang.Long = null,
  name: String = null
)
case class TypeOverrideTest (
  bank: java.lang.Long = null,
  lastModified: java.time.LocalDate = null
)
case class TypeOverrideTest2 (
  intColAsString: String = null,
  decimal266ColMod: scala.math.BigDecimal = null
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
case class WithChildExtends (
  code: String = null,
  name: String = null,
  bank: List[WithChildExtendsBank] = Nil
)
case class WithChildExtendsBank (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
)
case class WithChildExtendsCrud1 /* extends WithChildExtends */ (
  // --- WithChildExtends
  code: String = null,
  name: String = null,
  // bank: List[WithChildExtendsBank] = Nil
  // --- WithChildExtendsCrud1
  /* override */ bank: List[WithChildExtendsCrud1Bank] = Nil
)
case class WithChildExtendsCrud1Bank /* extends WithChildExtendsBank */ (
  // --- WithChildExtendsBank
  id: java.lang.Long = null,
  code: String = null,
  name: String = null,
  // --- WithChildExtendsCrud1Bank
  extra: String = null
)
case class WithChildExtendsCrud2 /* extends WithChildExtends */ (
  // --- WithChildExtends
  code: String = null,
  name: String = null,
  // bank: List[WithChildExtendsBank] = Nil
  // --- WithChildExtendsCrud2
  extra: String = null,
  /* override */ bank: List[WithChildExtendsCrud2Bank] = Nil
)
case class WithChildExtendsCrud2Bank /* extends WithChildExtendsBank */ (
  // --- WithChildExtendsBank
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
  // --- WithChildExtendsCrud2Bank
)
case class WithChildExtendsCrud3 /* extends WithChildExtends */ (
  // --- WithChildExtends
  code: String = null,
  name: String = null,
  // bank: List[WithChildExtendsBank] = Nil
  // --- WithChildExtendsCrud3
  extra: String = null,
  /* override */ bank: List[WithChildExtendsBank] = Nil
)
case class WithChildNoIndent1 (
  code: String = null,
  name: String = null,
  bank: List[WithChildNoIndent1Bank] = Nil
)
case class WithChildNoIndent1Bank (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
)
case class WithChildNoIndent2 (
  code: String = null,
  name: String = null,
  bank: List[WithChildNoIndent2Bank] = Nil
)
case class WithChildNoIndent2Bank (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
)
case class WithChildOrderByAsc (
  code: String = null,
  bank: List[WithChildOrderByAscBank] = Nil
)
case class WithChildOrderByAscBank (
  name: String = null
)
case class WithChildOrderByDesc (
  code: String = null,
  bank: List[WithChildOrderByDescBank] = Nil
)
case class WithChildOrderByDescBank (
  name: String = null
)
case class WithChildSaveOptions (
  code: String = null,
  name: String = null,
  bank: List[WithChildSaveOptionsBank] = Nil
)
case class WithChildSaveOptionsBank (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
)
case class WithEmptyChildSaveTo (
  singleChild: WithEmptyChildSaveToSingleChild = null
)
case class WithEmptyChildSaveToSingleChild (
  childName: String = null
)
case class WithJoinToParent (
  id: java.lang.Long = null,
  name: String = null,
  parents: List[WithJoinToParentParents] = Nil
)
case class WithJoinToParentParents (
  id: java.lang.Long = null,
  name: String = null
)
case class WithJoinToParentPlus (
  id: java.lang.Long = null,
  name: String = null,
  parents: List[WithJoinToParentPlusParents] = Nil
)
case class WithJoinToParentPlusParents (
  id: java.lang.Long = null,
  name: String = null
)
case class WithManyKnownKeys (
  id: java.lang.Long = null,
  code: String = null,
  name: String = null
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
case class WithOptionalFields (
  opt: Option[String] = None,
  optComplex: Option[WithOptionalFieldsOptComplex] = None,
  optComplexSeq: Option[List[WithOptionalFieldsOptComplexSeq]] = None
)
case class WithOptionalFieldsOptComplex (
  name: Option[String] = None
)
case class WithOptionalFieldsOptComplexSeq (
  name: Option[String] = None
)
case class WithSaveOptions (
  readOnly: String = null,
  insertOnly: String = null,
  updateOnly: String = null,
  bank: List[WithSaveOptionsBank] = Nil,
  bankRo: List[WithSaveOptionsBankRo] = Nil
)
case class WithSaveOptionsBank (
  id: java.lang.Long = null,
  code: String = null
)
case class WithSaveOptionsBankRo (
  id: java.lang.Long = null,
  code: String = null
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
  `type`: String = null,
  type_ : String = null
)
// end
