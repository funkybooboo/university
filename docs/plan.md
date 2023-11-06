# Objectives
- Play with Lambda function
- Play with API Gateway

# Overview
Continuing the Widget Application (worked on in HW2 and HW3), I will be writing my own producer program. 
The producer will be a lambda function that will handle RESTful request that comes though API Gateway. 
When the lambda receives a request, it will transform it into a widget request and place it into the request queue used in HW3.

Feel free to change the consumer program or SQS queue made in HW3. But this shouldn't be necessary.

# Instructions
## Step 1—Design the API and Lambda Handler
Design the details of the RESTful API and Lambda function. 
- Keep your design simple, easy to test, maintain, and scale.
- The only purpose of this API is to allow any client to submit a Widget Request. SO you only need to have an HTTP POST method.
- Consider passing the JSON serialization of a Widget Request, i.e., a JSON string as the body of the POST method request.
- The Lambda function must accept an incoming Widget Request event from API Gateway, validate the request, then place it into the queue.
- In the Lambda function, keep the actual event handler function as simple as possible. Put most of the logic in other helper functions. This will help you be able to easily test your logic independent of the API Gateway.

## Step2-Implement and Test your Widget Request Handler
