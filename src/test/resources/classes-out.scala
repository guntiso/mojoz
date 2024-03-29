package some.pack

class Account extends DtoWithId {
  var id: java.lang.Long = null
  var bankId: java.lang.Long = null
  var billingAccount: String = null
  var currency: List[String] = Nil
}
class AccountDetails extends Account with Dto {
  var bankCode: String = null
  var bankName: String = null
  // override var billingAccount: String = null
}
class BankListRow extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var countryCode: String = null
  var countryName: String = null
  var name: String = null
}
class CardinalityOverideTest extends Dto {
  var name: String = null
  var surname: String = null
}
class ExpressionTest1 extends DtoWithId {
  var id: java.lang.Long = null
  var bank: String = null
}
class NamedInlineView extends Dto {
  var childId: java.lang.Long = null
  var childName: String = null
}
class NamedInlineView2 extends Dto {
  var childName: String = null
}
class NamedInlineView3 extends Dto {
  var childName: String = null
}
class NoFields extends Dto {
}
class NoFieldsB extends Dto {
}
class NoFieldsExtendedB extends NoFieldsB with DtoWithId {
  var id: java.lang.Long = null
}
class NoFieldsExtendedWithFields extends NoFields with DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
  var countryName: String = null
}
class NoTable extends Dto {
  var someId: java.lang.Long = null
  var someName: String = null
  var someDate: java.time.LocalDate = null
  var someDateTime: java.time.LocalDateTime = null
  var isBoolean: java.lang.Boolean = null
  var isBooleanExpr: java.lang.Boolean = null
  var isNotBoolean: java.time.LocalDate = null
}
class OneFielder extends Dto {
  var theOne: String = null
}
class OtherDbPersonView extends DtoWithId {
  var id: java.lang.Long = null
  var fullName: String = null
}
class Person extends Dto {
  var name: String = null
  var surname: String = null
  var motherName: String = null
  var fatherName: String = null
  var maternalGrandmotherName: String = null
  var maternalGrandfatherName: String = null
  var maternalGrandfatherSurname: String = null
  var fatherMotherName: String = null
  var fatherFatherName: String = null
  var fatherFatherFatherSurname: String = null
}
class ReferencedColumnTypesTest extends Dto {
  var field1: String = null
  var field2: java.lang.Integer = null
}
class ResolverTest1 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
}
class ResolverTest2 extends Dto {
  var account: String = null
  var currencyName: String = null
  var accountBankCode: String = null
  var accountBankNameEng: String = null
}
class ResolverTest3 extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
}
class ResolverTest4 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class ResolverTest5 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class ResolverTest6 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class ResolverTest7 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var country: String = null
}
class ResolverTest7b extends DtoWithId {
  var id: java.lang.Long = null
  var country: String = null
}
class ResolverTest8 extends DtoWithId {
  var id: java.lang.Long = null
  var bank: String = null
}
class ResolverTest8b extends DtoWithId {
  var id: java.lang.Long = null
  var bankId: java.lang.Long = null
}
class ResolverTest9 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
}
class SchemaTest1 extends Dto {
  var t1Id: java.lang.Long = null
  var t1Name: String = null
}
class SchemaTest2 extends Dto {
  var id: java.lang.Long = null
  var testSchema1TestTable1Name: String = null
}
class SchemaTest3 extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class TypeOverrideTest extends Dto {
  var bank: java.lang.Long = null
  var lastModified: java.time.LocalDate = null
}
class TypeOverrideTest2 extends Dto {
  var intColAsString: String = null
  var decimal266ColMod: scala.math.BigDecimal = null
}
class WithAnonymousInlineView extends Dto {
  var someId: java.lang.Long = null
  var someName: String = null
  var someChildren: List[WithAnonymousInlineViewSomeChildren] = Nil
}
class WithAnonymousInlineViewSomeChildren extends Dto {
  var childId: java.lang.Long = null
  var childName: String = null
}
class WithChildExtends extends Dto {
  var code: String = null
  var name: String = null
  var bank: List[WithChildExtendsBank] = Nil
}
class WithChildExtendsBank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class WithChildExtendsCrud1 extends /* WithChildExtends */ Dto {
  // --- WithChildExtends
  var code: String = null
  var name: String = null
  // var bank: List[WithChildExtendsBank] = Nil
  // --- WithChildExtendsCrud1
  /* override */ var bank: List[WithChildExtendsCrud1Bank] = Nil
}
class WithChildExtendsCrud1Bank extends WithChildExtendsBank with Dto {
  var extra: String = null
}
class WithChildExtendsCrud2 extends /* WithChildExtends */ Dto {
  // --- WithChildExtends
  var code: String = null
  var name: String = null
  // var bank: List[WithChildExtendsBank] = Nil
  // --- WithChildExtendsCrud2
  var extra: String = null
  /* override */ var bank: List[WithChildExtendsCrud2Bank] = Nil
}
class WithChildExtendsCrud2Bank extends WithChildExtendsBank with Dto {
}
class WithChildExtendsCrud3 extends WithChildExtends with Dto {
  var extra: String = null
  // override var bank: List[WithChildExtendsBank] = Nil
}
class WithChildNoIndent1 extends Dto {
  var code: String = null
  var name: String = null
  var bank: List[WithChildNoIndent1Bank] = Nil
}
class WithChildNoIndent1Bank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class WithChildNoIndent2 extends Dto {
  var code: String = null
  var name: String = null
  var bank: List[WithChildNoIndent2Bank] = Nil
}
class WithChildNoIndent2Bank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class WithChildOrderByAsc extends Dto {
  var code: String = null
  var bank: List[WithChildOrderByAscBank] = Nil
}
class WithChildOrderByAscBank extends Dto {
  var name: String = null
}
class WithChildOrderByDesc extends Dto {
  var code: String = null
  var bank: List[WithChildOrderByDescBank] = Nil
}
class WithChildOrderByDescBank extends Dto {
  var name: String = null
}
class WithChildSaveOptions extends Dto {
  var code: String = null
  var name: String = null
  var bank: List[WithChildSaveOptionsBank] = Nil
}
class WithChildSaveOptionsBank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class WithEmptyChildSaveTo extends Dto {
  var singleChild: WithEmptyChildSaveToSingleChild = null
}
class WithEmptyChildSaveToSingleChild extends Dto {
  var childName: String = null
}
class WithJoinToParent extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var parents: List[WithJoinToParentParents] = Nil
}
class WithJoinToParentParents extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class WithJoinToParentPlus extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
  var parents: List[WithJoinToParentPlusParents] = Nil
}
class WithJoinToParentPlusParents extends DtoWithId {
  var id: java.lang.Long = null
  var name: String = null
}
class WithManyKnownKeys extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var name: String = null
}
class WithNamedInlineView extends Dto {
  var someId: java.lang.Long = null
  var someName: String = null
  var someChildren: List[NamedInlineView] = Nil
}
class WithNamedInlineView2 extends Dto {
  var someChildren: List[NamedInlineView2] = Nil
}
class WithNamedInlineView3 extends Dto {
  var singleChild: NamedInlineView3 = null
}
class WithOptionalFields extends Dto {
  var opt: Option[String] = None
  var optComplex: Option[WithOptionalFieldsOptComplex] = None
  var optComplexSeq: Option[List[WithOptionalFieldsOptComplexSeq]] = None
}
class WithOptionalFieldsOptComplex extends Dto {
  var name: Option[String] = None
}
class WithOptionalFieldsOptComplexSeq extends Dto {
  var name: Option[String] = None
}
class WithSaveOptions extends Dto {
  var readOnly: String = null
  var insertOnly: String = null
  var updateOnly: String = null
  var bank: List[WithSaveOptionsBank] = Nil
  var bankRo: List[WithSaveOptionsBankRo] = Nil
}
class WithSaveOptionsBank extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
}
class WithSaveOptionsBankRo extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
}
class WithScalaKeywords extends Dto {
  var `this`: String = null
  var `super`: String = null
  var `new`: String = null
  var `true`: String = null
  var `lazy`: String = null
  var `null`: String = null
  var `case`: String = null
  var `final`: String = null
  var `type`: String = null
  var type_ : String = null
}
// end
