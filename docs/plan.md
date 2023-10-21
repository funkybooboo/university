#### 1. SQS Integration
Allow user to choose if S3 or SQS should be used for pulling widgets.

If SQS is used:
retrieve a message containing a request, process that request, 
delete the message in the queue using the message receipt handle. 

up to 10 messages at a time can be read. 
using long polling in a single receive-message operation.
return one at a time with caching.

#### 2. Process new kinds of widgets
Create -> done in that last HW
Delete
Update

#### 3. run multiple consumers
allow many consumers to be able to run at the same time


#### 4. docker
create dockerfile that: 
1. specifies your runtime environment 
2. copy your program into the container
3. specify a command that will run your program
4. has access to aws credentials

test your docker image 



#### turn in
1. screenshot of the SQS on the AWS console
2. screenshot of at least two consumers running concurrently
3. screenshot of git repo
4. screenshot the running of a container
5. screenshot of pushed docker image
6. screenshot cluster view of containers
7. answers to questions on step 8
8. archive file of my entire project

