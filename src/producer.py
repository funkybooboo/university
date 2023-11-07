import boto3
import json
import logging


class Session:
    def __init__(self):
        self.region = 'us-east-1'
        self.sqs = boto3.client('sqs', region_name=self.region)
        self.queue = 'cs5260-requests'
        self.queue_url = self.get_queue_url()

    def get_queue_url(self):
        account_number = boto3.client('sts').get_caller_identity()['Account']
        return f'https://sqs.{self.region}.amazonaws.com/{account_number}/{self.queue}'


def lambda_handler(event, context):
    setup_logging()
    logging.info(event)
    logging.info(context)
    try:
        widget = json.loads(event.get('body'))
        validate_widget(widget)
        return push_widget_sqs(widget, Session())
    except (ValueError, KeyError) as e:
        return error_response(e, 'Invalid widget data')


def setup_logging():
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[logging.FileHandler('widget-request.log'), logging.StreamHandler()]
    )


def validate_widget(widget):
    widget_type = widget.get('type')
    if widget_type == 'create':
        validate_create(widget)
    elif widget_type == 'update':
        validate_update(widget)
    elif widget_type == 'delete':
        validate_delete(widget)
    else:
        raise KeyError('Invalid widget type')


def validate_create(widget):
    required_fields = ['widgetId', 'owner', 'label', 'description']
    for field in required_fields:
        if not widget.get(field):
            raise KeyError(f'Missing required field: {field}')


def validate_update(widget):
    required_fields = ['widgetId', 'owner', 'description']
    for field in required_fields:
        if not widget.get(field):
            raise KeyError(f'Missing required field: {field}')


def validate_delete(widget):
    required_fields = ['widgetId', 'owner']
    for field in required_fields:
        if not widget.get(field):
            raise KeyError(f'Missing required field: {field}')


def push_widget_sqs(widget, session):
    try:
        message_body = json.dumps(widget)
        response = session.sqs.send_message(QueueUrl=session.queue_url, MessageBody=message_body)
        if response.get('MessageId'):
            return success_response('success widget push sqs')
        return error_response('Failed to push widget to SQS', 'Failed to send message to SQS')
    except Exception as e:
        return error_response(e, 'Failed to send message to SQS')


def success_response(message):
    logging.info(message)
    return {
        'statusCode': 200,
        'body': {'message': message}
    }


def error_response(e, message):
    logging.error(message)
    logging.error(e)
    return {
        'statusCode': 400,  # You may adjust the status code as needed
        'body': {'message': message, 'error': str(e)}
    }
