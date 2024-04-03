package main

import (
	"fmt"

	"github.com/aws/aws-lambda-go/events"
	"github.com/aws/aws-lambda-go/lambda"
)

func Handler(request events.APIGatewayProxyRequest) (events.APIGatewayProxyResponse, error) {
	fmt.Println("Received body: ", request.Body)

	response := events.APIGatewayProxyResponse{
		Body:       "you said " + request.Body,
		StatusCode: 200,
	}

	return response, nil
}

func main() {
	lambda.Start(Handler)
}
