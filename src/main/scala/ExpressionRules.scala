package mojoz.metadata.in

trait ExpressionRules {
  def isExpressionFilterable(fieldExpression: String): Boolean
}

package rules {

trait AllExpressionsFilterable extends ExpressionRules {
  override def isExpressionFilterable(fieldExpression: String) = true
}
}