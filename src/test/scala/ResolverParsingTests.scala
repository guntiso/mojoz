import scala.io.Source
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.io._
import mojoz.metadata.out._
import java.io.PrintWriter
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }

// TODO these tests to text-based parse results (json/yaml/...)?
class ResolverParsingTests extends FlatSpec with Matchers {
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val viewDefs = YamlViewDefLoader(tableMd, mdDefs).viewDefs
  val xViewDefs = YamlViewDefLoader(tableMd, mdDefs).extendedViewDefs

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
    field(t4, "name").saveTo should be(null)
    field(t4, "name").resolver should be("'My bank'")

    val t5 = xViewDefs("resolver_test_5")

    field(t5, "name").name should be("name")
    field(t5, "name").alias should be(null)
    field(t5, "name").table should be("bank")
    field(t5, "name").tableAlias should be(null)
    field(t5, "name").isExpression should be(false)
    field(t5, "name").expression should be(null)
    field(t5, "name").saveTo should be(null)
    field(t5, "name").resolver should be("_ || ' saved'")
  }
  def field(v: ViewDef[FieldDef[Type]], n: String) =
    v.fields.filter(f => Option(f.alias).getOrElse(f.name) == n).head
}
