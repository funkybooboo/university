import boto3
import json


class Session:
    def __init__(self, region):
        self.region = region
        self.sqs = boto3.client('sqs', region_name=self.region)
        self.queue = 'cs5260-requests'
        self.account_number = boto3.client('sts').get_caller_identity()['Account']
        self.queue_url = f'https://sqs.{self.region}.amazonaws.com/{self.account_number}/{self.queue}'


def lambda_handler(event, context):
    try:
        print(f'event: {event}')
        print(f'context: {context}')
        request_id = context.aws_request_id
        print(f'request_id: {request_id}')
        region = context.invoked_function_arn.split(":")[3]
        print(f'region: {region}')

        if not event:
            return error_response('Event is None')

        widget = event.get('body')
        if not widget:
            return error_response('Widget is None')

        if isinstance(widget, str):
            widget = json.loads(widget)

        if not isinstance(widget, dict):
            return error_response(f'Widget is not a dictionary: {type(widget)}')

        widget['requestId'] = request_id
        print(f'widget: {widget}')

        validate_widget(widget)
        print('valid widget')

        session = Session(region)
        return push_widget_sqs(widget, session)

    except Exception as e:
        return error_response(str(e))


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
    for k, v in fields.items():
        check_field(widget, k, v)


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
    error = str(error).strip("'")
    print(error)
    return json.dumps({
        'statusCode': 400,
        'body': {'error': error}
    })
