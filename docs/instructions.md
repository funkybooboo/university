# Objectives
- Play with Lambda function
- Play with API Gateway

# Overview
Continuing the Widget Application (worked on in HW2 and HW3), I will be writing my own producer program. 
The producer will be a lambda function that will handle RESTful request that comes though API Gateway. 
When the lambda receives a request, it will transform it into a widget request and place it into the request queue used in HW3.

Feel free to change the consumer program or SQS queue made in HW3. But this shouldn't be necessary.

# Instructions
## Step 1 — Design the API and Lambda Handler
Design the details of the RESTful API and Lambda function. 
- Keep your design simple, easy to test, maintain, and scale.
- The only purpose of this API is to allow any client to submit a Widget Request. You only need to have an HTTP POST method.
- Consider passing the JSON serialization of a Widget Request, i.e., a JSON string as the body of the POST method request.
- The Lambda function must accept an incoming Widget Request event from API Gateway, validate the request, then place it into the queue.
- In the Lambda function, keep the actual event handler function as simple as possible. Put most of the logic in other helper functions. This will help you be able to easily test your logic independent of the API Gateway.

## Step 2 - Implement and Test your Widget Request Handler
Implement the widget-request-handler logic and unit-tests. 
Use Git and commit frequently. At the end, I will turn in a screenshot of my git log.
Tips:
- Keep your logic simple.
- Build good test cases.
- Output log messages that will help debug the code.

## Step 3 - Deploy the Lambda function
Deploy your widget-request-handler logic to AWS lambda by any means, including the AWS Lambda Console.

Test your new lambda function with test API Gateway Events that simulate create, update, and delete widget request coming in from the API.
The bodies of these test events need to be consistent with my design. Save these test events with your lambda function in the AWS lambda console. 
Also, copy them into your project directory.
Take three or more screenshots that illustrate the working lambda function using the test events.

## Step 4 - Create and Test a RESTful API
Using any mechanism, create a RESTful API that will be the front end for your widget-request-handler Lambda function.

Test the API in the AWS API Gateway Console using the appropriate HTTP methods and event bodies. 
The testing should try method requests that submit, create, update, and delete requests.

Take three or more screenshots that show the API processing create, update, and delete requests.

## Step 5 - Deploy the API and additional testing
Deploy the API to production and label with the "prod" stage. Test using a generic HTTP client like Postman.

Take three or more screenshots that show the API processing create, update, and delete requests. 

## Submission
Submission must include:
- All screen shots mentioned 
- An archive file of the entire project, excluding build/run-time artifacts.

