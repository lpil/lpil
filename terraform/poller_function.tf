resource "aws_iam_role" "lambda_exec_role" {
  name = "lambda_exec_role"

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

resource "aws_lambda_function" "poller" {
  function_name    = "blondie_poller"
  handler          = "main.handler"
  runtime          = "nodejs8.10"
  filename         = "dist/function.zip"
  source_code_hash = "${base64sha256(file("dist/function.zip"))}"
  role             = "${aws_iam_role.lambda_exec_role.arn}"
}
