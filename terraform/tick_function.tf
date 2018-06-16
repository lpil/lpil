# The tick function runs periodically, checking if we need to send some alerts.

module "tick_function" {
  env           = "${var.env}"
  source        = "./lambda_function"
  function_name = "tick"
  description   = "Periodically checks if we need to send alerts"
}

resource "aws_cloudwatch_event_rule" "tick_event" {
  name                = "${var.env}_tick_event"
  description         = "Periodic event that triggers the checking for alerts to send"
  schedule_expression = "rate(5 minutes)"
}

resource "aws_cloudwatch_event_target" "tick_event_lambda_target" {
  target_id = "${var.env}_tick_event_lambda_target"
  rule      = "${aws_cloudwatch_event_rule.tick_event.name}"
  arn       = "${module.tick_function.function_arn}"

  input = <<EOF
{
  "someKey": "Hello, world!"
}
EOF
}

resource "aws_lambda_permission" "allow_cloudwatch_to_call_tick_function" {
  statement_id  = "AllowExecutionFromCloudWatch"
  action        = "lambda:InvokeFunction"
  function_name = "${module.tick_function.function_name}"
  principal     = "events.amazonaws.com"
  source_arn    = "${aws_cloudwatch_event_rule.tick_event.arn}"
}
