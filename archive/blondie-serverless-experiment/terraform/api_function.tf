# The API function servers our HTTP API.

module "api_function" {
  env           = "${var.env}"
  source        = "./lambda_function"
  function_name = "api"
  description   = "Serves the Blondie HTTP API"
}

resource "aws_api_gateway_rest_api" "api_function" {
  name        = "api_function"
  description = "Gateway for Blondie API function"
}

# The API gateway proxies all requests, regardless of the method and path

resource "aws_api_gateway_resource" "api_function_proxy" {
  rest_api_id = "${aws_api_gateway_rest_api.api_function.id}"
  parent_id   = "${aws_api_gateway_rest_api.api_function.root_resource_id}"

  # Allow any path
  path_part = "{proxy+}"
}

resource "aws_api_gateway_method" "api_function_proxy" {
  rest_api_id   = "${aws_api_gateway_rest_api.api_function.id}"
  resource_id   = "${aws_api_gateway_resource.api_function_proxy.id}"
  http_method   = "ANY"
  authorization = "NONE"
}

# The API gateway proxy calls the API lambda function

resource "aws_api_gateway_integration" "api_function_proxy" {
  rest_api_id = "${aws_api_gateway_rest_api.api_function.id}"
  resource_id = "${aws_api_gateway_method.api_function_proxy.resource_id}"
  http_method = "${aws_api_gateway_method.api_function_proxy.http_method}"

  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = "${module.api_function.invoke_arn}"
}

# The above proxy doesn't match the root path "/", so handle that too

resource "aws_api_gateway_method" "api_function_proxy_root" {
  rest_api_id   = "${aws_api_gateway_rest_api.api_function.id}"
  resource_id   = "${aws_api_gateway_rest_api.api_function.root_resource_id}"
  http_method   = "ANY"
  authorization = "NONE"
}

resource "aws_api_gateway_integration" "api_function_proxy_root" {
  rest_api_id = "${aws_api_gateway_rest_api.api_function.id}"
  resource_id = "${aws_api_gateway_method.api_function_proxy_root.resource_id}"
  http_method = "${aws_api_gateway_method.api_function_proxy_root.http_method}"

  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = "${module.api_function.invoke_arn}"
}

# Give the gateway permission to invoke the lambda

resource "aws_lambda_permission" "api_gateway_lambda_invocation" {
  statement_id  = "AllowAPIGatewayInvoke"
  action        = "lambda:InvokeFunction"
  function_name = "${module.api_function.function_arn}"
  principal     = "apigateway.amazonaws.com"

  # The /*/* portion grants access from any method on any resource
  # within the API Gateway "REST API".
  source_arn = "${aws_api_gateway_deployment.api.execution_arn}/*/*"
}

# Boom, expose the API to the world

resource "aws_api_gateway_deployment" "api" {
  depends_on = [
    "aws_api_gateway_integration.api_function_proxy",
    "aws_api_gateway_integration.api_function_proxy_root",
  ]

  rest_api_id = "${aws_api_gateway_rest_api.api_function.id}"
  stage_name  = "${var.env}"
}

output "api_url" {
  value = "${aws_api_gateway_deployment.api.invoke_url}"
}
