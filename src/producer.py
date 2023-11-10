import boto3
import json


class Session:
    def __init__(self, region, queue):
        self.region = region
        self.queue = queue
        self.sqs = boto3.client('sqs', region_name=self.region)
        self.account_number = boto3.client('sts').get_caller_identity()['Account']
        self.queue_url = f'https://sqs.{self.region}.amazonaws.com/{self.account_number}/{self.queue}'


def lambda_handler(event, context):
    try:
        widget, session = get_event_data(event, context)
        validate_widget(widget)
        print('valid widget')
        return push_widget_sqs(widget, session)
    except Exception as e:
        return error_response(str(e))


def get_event_data(event, context):
    print(f'event: {event}')
    print(f'context: {context}')
    request_id = context.aws_request_id
    print(f'request_id: {request_id}')
    if not event:
        raise KeyError('Event is None')
    body = event.get('body')
    if not body:
        raise KeyError('Body is None')
    if isinstance(body, str):
        body = json.loads(body)
    if not isinstance(body, dict):
        raise KeyError(f'Body is not a dictionary: {type(body)}')
    widget = get_widget(body)
    session = get_session(body)
    widget['requestId'] = request_id
    print(f'widget: {widget}')
    return widget, session


def get_session(body):
    region = body.get('region')
    if not region:
        region = 'us-east-1'
    queue = body.get('queue')
    if not queue:
        queue = 'cs5260-requests'
    session = Session(region, queue)
    print('Got session info')
    return session

def get_widget(body):
    widget = body.get('widget')
    if not widget:
        raise KeyError('Widget is None')
    if isinstance(widget, str):
        widget = json.loads(widget)
    if not isinstance(widget, dict):
        raise KeyError(f'Widget is not a dictionary: {type(widget)}')
    print('Got widget')
    return widget


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
    fields = {
        'widgetId': str,
        'owner': str,
        'label': str,
        'description': str,
        'otherAttributes': list
    }
    check_fields(fields, widget)


def validate_update(widget):
    fields = {
        'widgetId': str,
        'owner': str,
        'description': str,
    }
    check_fields(fields, widget)


def validate_delete(widget):
    fields = {
        'widgetId': str,
        'owner': str,
    }
    check_fields(fields, widget)


def check_fields(fields, widget):
    for field, field_type in fields.items():
        check_field(widget, field, field_type)


def check_field(widget, field, field_type):
    if not widget.get(field):
        raise KeyError(f'missing required field: {field}')
    if not isinstance(widget[field], field_type):
        raise KeyError(f'required field: {field}. required type: {field_type}. got type: {type(widget[field])}')


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
    print(message)
    return json.dumps({
        'statusCode': 200,
        'body': {'message': message}
    })


def error_response(error):
    error = str(error).strip("'").strip('"')
    print(error)
    return json.dumps({
        'statusCode': 400,
        'body': {'error': error}
    })
