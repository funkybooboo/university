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
        logging.info('got widget from event')
        validate_widget(widget)
        logging.info('valid widget')
        return push_widget_sqs(widget, Session())
    except (ValueError, KeyError) as e:
        return error_response(e)


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
        raise KeyError('invalid widget type')


def validate_create(widget):
    required_str_fields = ['widgetId', 'owner', 'label', 'description']
    for field in required_str_fields:
        if not widget.get(field) or not type(widget[field]) == str:
            raise KeyError(f'missing required field: {field}')
    if not widget.get('otherAttributes') or not type(widget['otherAttributes']) == list:
        raise KeyError(f'missing required field: otherAttributes')


def validate_update(widget):
    required_fields = ['widgetId', 'owner', 'description']
    for field in required_fields:
        if not widget.get(field) or not type(widget[field]) == str:
            raise KeyError(f'missing required field: {field}')


def validate_delete(widget):
    required_fields = ['widgetId', 'owner']
    for field in required_fields:
        if not widget.get(field) or not type(widget[field]) == str:
            raise KeyError(f'missing required field: {field}')


def push_widget_sqs(widget, session):
    try:
        message_body = json.dumps(widget)
        response = session.sqs.send_message(QueueUrl=session.queue_url, MessageBody=message_body)
        if response.get('MessageId'):
            return success_response('success widget push sqs')
        return error_response('fail widget push sqs')
    except Exception as e:
        return error_response(e)


def success_response(message):
    logging.info(message)
    return {
        'statusCode': 200,
        'body': {'message': message}
    }


def error_response(e):
    logging.error(e)
    return {
        'statusCode': 400,
        'body': {'error': str(e)}
    }
