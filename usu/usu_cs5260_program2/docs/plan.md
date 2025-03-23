# Plan for consumer program

## 1. Command line interface
store in either bucket 3 or in widgets dynamodb table
use can specify which one they would like via command line args

usu-cs5260-nate-requests will be (bucket 2) the place where widget requests are found.

usu-cs5360-nate-web will be (bucket 3) where the requests are filled out to.

name of object in bucket is called the key

## 2. Read loop
periodically try to read a single widget request from bucket 2 in key order. smallest to largest. 

if it finds a request it should delete the request from the bucket and then process the request. then go back to trying to read another request.

if there is no requests in the bucket then wait for an amount of time like 100ms

sudocode from instructor

Loop until some stop condition met
    Try to get request
    If got request
        Process request
    Else
        Wait a while (100ms)
End loop

## 3. types

There are three kinds of widgets but for this assignment only one kind needs to be handled

Widget Create Request:
When the Consumer process a widget Creat Request, it will creat the specified widget and store it in either bucket 3 or the DynamoDb table.

## 4. Storage

the widget request needs to have all information stored

If the widget is being stored in the bucket then the json object should be converted to a string.
Its key should be based on the following pattern:
widgets/<owner>/<widget id>/json_string
<owner> replace spaces with dashes and convert to lower case.

If the widget is being stored in the dynamodb. Place the widget with all its item in the db as an item with all its attributes.


## 5. Notes

In a future assignment your code should be able to handle other types of widget requests. As well as being able to change what score the requests are coming from. Such as a queue.
