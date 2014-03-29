package metadata

trait ExpressionRules {
  def isExpressionFilterable(fieldExpression: String): Boolean
}

trait AllExpressionsFilterable extends ExpressionRules {
  override def isExpressionFilterable(fieldExpression: String) = true
}
