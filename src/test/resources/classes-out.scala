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
}
class BankListRow extends DtoWithId {
  var id: java.lang.Long = null
  var code: String = null
  var countryCode: String = null
  var countryName: String = null
  var name: String = null
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
  var someDate: java.sql.Date = null
  var someDateTime: java.sql.Timestamp = null
  var isBoolean: java.lang.Boolean = null
  var isBooleanExpr: java.lang.Boolean = null
  var isNotBoolean: java.sql.Date = null
}
class OneFielder extends Dto {
  var theOne: String = null
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
class ResolverTest1 extends DtoWithId {
  var id: java.lang.Long = null
  var mother: String = null
  var father: String = null
}
class ResolverTest2 extends Dto {
  var account: String = null
  var currencyName: String = null
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
class TypeOverrideTest extends Dto {
  var bank: java.lang.Long = null
  var lastModified: java.sql.Date = null
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
