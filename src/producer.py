import boto3
import json
import logging


def lambda_handler(event, context):
    pass
    # find out what type of request it is: update, create, delete
    # validate if the type is a valid request, has the right attributes
    # send message to SQS with the found validated information
