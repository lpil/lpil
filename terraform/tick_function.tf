# The tick function runs periodically, checking if we need to send some alerts.

resource "aws_iam_role" "tick_role" {
  name = "${var.env}_tick_role"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_cloudwatch_event_rule" "tick_event" {
  name                = "${var.env}_tick_event"
  description         = "Periodic event that triggers the checking for alerts to send"
  schedule_expression = "rate(1 minute)"
}

resource "aws_cloudwatch_event_target" "tick_event_lambda_target" {
  target_id = "${var.env}_tick_event_lambda_target"
  rule      = "${aws_cloudwatch_event_rule.tick_event.name}"
  arn       = "${aws_lambda_function.tick_function.arn}"

  input = <<EOF
{
  "someKey": "Hello, world!"
}
EOF
}

resource "aws_lambda_function" "tick_function" {
  function_name    = "${var.env}_blondie_tick"
  handler          = "main.handler"
  runtime          = "nodejs8.10"
  filename         = "dist/function.zip"
  source_code_hash = "${base64sha256(file("dist/function.zip"))}"
  role             = "${aws_iam_role.tick_role.arn}"

  tags {
    Name    = "${var.env}_blondie_tick"
    Env     = "${var.env}"
    Project = "blondie"
    Desc    = "Periodically checks if we need to send alerts"
  }
}

resource "aws_lambda_permission" "allow_cloudwatch_to_call_tick_function" {
  statement_id  = "AllowExecutionFromCloudWatch"
  action        = "lambda:InvokeFunction"
  function_name = "${aws_lambda_function.tick_function.function_name}"
  principal     = "events.amazonaws.com"
  source_arn    = "${aws_cloudwatch_event_rule.tick_event.arn}"
}

resource "aws_iam_policy" "tick_function_policy" {
  name        = "${var.env}_lambda_logging"
  path        = "/"
  description = "IAM policy for logging from a lambda"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*",
      "Effect": "Allow"
    }
  ]
}
EOF
}

resource "aws_iam_role_policy_attachment" "tick_function_policy" {
  role       = "${aws_iam_role.tick_role.name}"
  policy_arn = "${aws_iam_policy.tick_function_policy.arn}"
}
