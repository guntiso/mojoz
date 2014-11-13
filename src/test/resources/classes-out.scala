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
class TypeOverrideTest extends Dto {
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
// end
