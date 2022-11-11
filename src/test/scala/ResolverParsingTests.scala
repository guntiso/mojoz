import scala.io.Source
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.mojoz.metadata._
import org.mojoz.metadata.in._
import org.mojoz.metadata.io._
import org.mojoz.metadata.out._
import java.io.PrintWriter

// TODO these tests to text-based parse results (json/yaml/...)?
class ResolverParsingTests extends FlatSpec with Matchers {
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val xViewDefs = YamlViewDefLoader(tableMd, mdDefs).nameToViewDef

  "parsing for save-to and resolver" should "work properly" in {

    val t1 = xViewDefs("resolver_test_1")

    field(t1, "mother").name should be("mother")
    field(t1, "mother").alias should be(null)
    field(t1, "mother").table should be(null)
    field(t1, "mother").tableAlias should be(null)
    field(t1, "mother").isExpression should be(true)
    field(t1, "mother").expression should be("mother.name || mother.surname")
    field(t1, "mother").saveTo should be("mother_id")
    field(t1, "mother").resolver should be(null)

    field(t1, "father").name should be("father")
    field(t1, "father").alias should be(null)
    field(t1, "father").table should be(null)
    field(t1, "father").tableAlias should be(null)
    field(t1, "father").isExpression should be(true)
    field(t1, "father").expression should be("father.name || father.surname")
    field(t1, "father").saveTo should be("father_id")
    field(t1, "father").resolver should be("person[name || surname = _]{id}")

    val t2 = xViewDefs("resolver_test_2")

    field(t2, "account").name should be("billing_account")
    field(t2, "account").alias should be("account")
    field(t2, "account").table should be("account")
    field(t2, "account").tableAlias should be(null)
    field(t2, "account").isExpression should be(false)
    field(t2, "account").expression should be(null)
    field(t2, "account").saveTo should be("account_id")
    field(t2, "account").resolver should be(null)

    field(t2, "currency_name").name should be("name")
    field(t2, "currency_name").alias should be("currency_name")
    field(t2, "currency_name").table should be("currency")
    field(t2, "currency_name").tableAlias should be(null)
    field(t2, "currency_name").isExpression should be(false)
    field(t2, "currency_name").expression should be(null)
    field(t2, "currency_name").saveTo should be("currency_code")
    field(t2, "currency_name").resolver should be(null)

    val t3 = xViewDefs("resolver_test_3")

    field(t3, "code").name should be("code")
    field(t3, "code").alias should be(null)
    field(t3, "code").table should be(null)
    field(t3, "code").tableAlias should be(null)
    field(t3, "code").isExpression should be(true)
    field(t3, "code").expression should be(null)
    field(t3, "code").saveTo should be("bank_id")
    field(t3, "code").resolver should be(null)

    val t4 = xViewDefs("resolver_test_4")

    field(t4, "name").name should be("name")
    field(t4, "name").alias should be(null)
    field(t4, "name").table should be("bank")
    field(t4, "name").tableAlias should be(null)
    field(t4, "name").isExpression should be(false)
    field(t4, "name").expression should be(null)
    field(t4, "name").saveTo should be("name")
    field(t4, "name").resolver should be("'My bank'")

    val t5 = xViewDefs("resolver_test_5")

    field(t5, "name").name should be("name")
    field(t5, "name").alias should be(null)
    field(t5, "name").table should be("bank")
    field(t5, "name").tableAlias should be(null)
    field(t5, "name").isExpression should be(false)
    field(t5, "name").expression should be(null)
    field(t5, "name").saveTo should be("name")
    field(t5, "name").resolver should be("_ || ' saved'")

    val t6 = xViewDefs("resolver_test_6")

    field(t6, "name").name should be("name")
    field(t6, "name").alias should be(null)
    field(t6, "name").table should be(null)
    field(t6, "name").tableAlias should be(null)
    field(t6, "name").isExpression should be(true)
    field(t6, "name").expression should be("name || '.'")
    field(t6, "name").saveTo should be("name")
    field(t6, "name").resolver should be(null)

    val t7 = xViewDefs("resolver_test_7")

    field(t7, "name").name should be("name")
    field(t7, "name").alias should be(null)
    field(t7, "name").table should be("bank")
    field(t7, "name").tableAlias should be(null)
    field(t7, "name").isExpression should be(false)
    field(t7, "name").expression should be(null)
    field(t7, "name").saveTo should be("name")
    field(t7, "name").resolver should be(null)

    field(t7, "country").name should be("name")
    field(t7, "country").alias should be("country")
    field(t7, "country").table should be("country")
    field(t7, "country").tableAlias should be(null)
    field(t7, "country").isExpression should be(false)
    field(t7, "country").expression should be(null)
    field(t7, "country").saveTo should be("country_code")
    field(t7, "country").resolver should be(null)

    val t7b = xViewDefs("resolver_test_7b")

    field(t7b, "country").name should be("name")
    field(t7b, "country").alias should be("country")
    field(t7b, "country").table should be("country")
    field(t7b, "country").tableAlias should be(null)
    field(t7b, "country").isExpression should be(false)
    field(t7b, "country").expression should be(null)
    field(t7b, "country").saveTo should be("country_code")
    field(t7b, "country").resolver should be("to_country_code(_)")

    val t8 = xViewDefs("resolver_test_8")

    field(t8, "bank").name should be("name")
    field(t8, "bank").alias should be("bank")
    field(t8, "bank").table should be("bank")
    field(t8, "bank").tableAlias should be(null)
    field(t8, "bank").isExpression should be(false)
    field(t8, "bank").expression should be(null)
    field(t8, "bank").saveTo should be("bank_id")
    field(t8, "bank").resolver should be(null)

    val t8b = xViewDefs("resolver_test_8b")

    field(t8b, "bank_id").name should be("bank_id")
    field(t8b, "bank_id").alias should be(null)
    field(t8b, "bank_id").table should be("account")
    field(t8b, "bank_id").tableAlias should be(null)
    field(t8b, "bank_id").isExpression should be(false)
    field(t8b, "bank_id").expression should be(null)
    field(t8b, "bank_id").saveTo should be("bank_id")
    field(t8b, "bank_id").resolver should be("33")

    val t9 = xViewDefs("resolver_test_9")

    field(t9, "mother").name should be("mother")
    field(t9, "mother").alias should be(null)
    field(t9, "mother").table should be(null)
    field(t9, "mother").tableAlias should be(null)
    field(t9, "mother").isExpression should be(true)
    field(t9, "mother").expression should be("mother.name || mother.surname")
    field(t9, "mother").saveTo should be("mother_id")
    field(t9, "mother").resolver should be(null)

    field(t9, "father").name should be("father")
    field(t9, "father").alias should be(null)
    field(t9, "father").table should be(null)
    field(t9, "father").tableAlias should be(null)
    field(t9, "father").isExpression should be(true)
    field(t9, "father").expression should be("father.name || ' ' || father.surname")
    field(t9, "father").saveTo should be("father_id")
    field(t9, "father").resolver should be(null)
  }
  def field(v: ViewDef, n: String) =
    v.field(n)
}
