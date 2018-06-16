resource "aws_iam_role" "function_role" {
  name = "${var.env}_${var.function_name}_role"

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

resource "aws_lambda_function" "function" {
  function_name    = "${var.env}_blondie_${var.function_name}"
  handler          = "${var.function_name}_function.handler"
  runtime          = "nodejs8.10"
  filename         = "dist/${var.function_name}_function.zip"
  source_code_hash = "${base64sha256(file("dist/${var.function_name}_function.zip"))}"
  role             = "${aws_iam_role.function_role.arn}"

  tags {
    Name    = "${var.env}_blondie_${var.function_name}"
    Env     = "${var.env}"
    Project = "blondie"
    Desc    = "${var.description}"
  }
}

resource "aws_iam_policy" "function_policy" {
  name        = "${var.env}_lambda_logging_${var.function_name}"
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

resource "aws_iam_role_policy_attachment" "function_policy_attachment" {
  role       = "${aws_iam_role.function_role.name}"
  policy_arn = "${aws_iam_policy.function_policy.arn}"
}
