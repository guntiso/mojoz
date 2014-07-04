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
class NoTable extends Dto {
  var someId: java.lang.Long = null
  var someName: String = null
  var someDate: java.sql.Date = null
  var someDateTime: java.sql.Timestamp = null
  var isBoolean: java.lang.Boolean = null
  var isBooleanExpr: java.lang.Boolean = null
  var isNotBoolean: java.sql.Date = null
}
// end
