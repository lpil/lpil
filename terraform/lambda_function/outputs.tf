output "function_name" {
  value = "${aws_lambda_function.function.function_name}"
}

output "function_arn" {
  value = "${aws_lambda_function.function.arn}"
}

output "invoke_arn" {
  value = "${aws_lambda_function.function.invoke_arn}"
}
