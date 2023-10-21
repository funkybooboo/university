import boto3
import time
import json
import logging
import argparse


def main():
    args = get_args()
    if args['log_level']:
        level = logging.DEBUG
    else:
        level = logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('consumer.log'),
            logging.StreamHandler()
        ]
    )
    logging.info('Start program')
    if args['pull_choice'] == 's3':
        consume(args, get_widget_s3)
    else:
        consume(args, get_widget_sqs)
    logging.info('End Program')
    logging.shutdown()


def consume(args, pull_widget):
    end = (time.time() * 1000) + (args['max_runtime'] * 1000)
    count = 0
    try:
        while (time.time() * 1000) < end and count < args['max_widget_pulls']:
            widget, widget_key = pull_widget(args)
            count += 1
            if widget is None:
                logging.info('Did not find a widget')
                time.sleep((args['inter_pull_delay'] / 1000))
                continue
            logging.info(f'Got widget: {widget_key}')
            push_widget(widget, widget_key, args)
    except Exception as e:
        logging.error('An error occurred:', exc_info=True)


def push_widget(widget, widget_key, args):
    try:
        if widget['type'] == 'create' and is_valid(widget):
            logging.info('processing valid create widget')
            create_widget(widget, args)
        elif widget['type'] == 'update':
            logging.info('processing valid update widget')
            update_widget(widget, args)
        elif widget['type'] == 'delete':
            logging.info('processing valid delete widget')
            delete_widget(widget, args)
        else:
            raise
    except Exception as e:
        logging.error(f'Bad Processing: {widget_key}')


def create_widget(widget, args):
    if args['push_choice'] == 's3':
        create_s3_object(widget, args)
    else:
        create_dynamodb_object(widget, args)


def update_widget(widget, args):
    if args['push_choice'] == 's3':
        update_s3_object(widget, args)
    else:
        update_dynamodb_object(widget, args)


def delete_widget(widget, args):
    if args['push_choice'] == 's3':
        delete_s3_object(widget, args)
    else:
        delete_dynamodb_object(widget, args)


def create_dynamodb_object(widget, args):
    dynamodb = boto3.resource('dynamodb', region_name=args['region'])
    table = dynamodb.Table(args['push_table'])
    item = {
        'id': widget['widgetId'],
        'owner': widget['owner'],
        'label': widget['label'],
        'description': widget['description']
    }
    for i in widget['otherAttributes']:
        item.update({i['name']: i['value']})
    table.put_item(Item=item)
    logging.info('successful put to dynamodb')


def create_s3_object(widget, args):
    s3_client = boto3.client('s3')
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    item_content = str(widget)
    s3_client.put_object(Bucket=args['push_bucket'], Key=object_key, Body=item_content)
    logging.info('successful put to s3')


def update_s3_object(widget, args):
    s3_client = boto3.client('s3')


def update_dynamodb_object(widget, args):
    dynamodb = boto3.resource('dynamodb', region_name=args['region'])
    table = dynamodb.Table(args['push_table'])


def delete_s3_object(widget, args):
    s3_client = boto3.client('s3')


def delete_dynamodb_object(widget, args):
    dynamodb = boto3.resource('dynamodb', region_name=args['region'])
    table = dynamodb.Table(args['push_table'])


def is_valid(widget):
    return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(
        widget['description']) == str


def get_widget_sqs(args):
    try:
        region = args['region']
        iam = boto3.client('iam')
        response = iam.list_account_aliases()
        if 'AccountAliases' in response:
            account = response['AccountAliases'][0]
        else:
            response = iam.get_user()
            account_arn = response['User']['Arn']
            account = account_arn.split(':')[4]
        queue = args['pull_queue']
        sqs = boto3.client('sqs', region_name=region)
        queue_url = f'https://sqs.{region}.amazonaws.com/{account}/{queue}'
        response = sqs.receive_message(
            QueueUrl=queue_url,
            MaxNumberOfMessages=1,
            WaitTimeSeconds=10
        )
        if 'Messages' in response:
            message = response['Messages'][0]
            receipt_handle = message['ReceiptHandle']
            widget_key = 'unknown'
            try:
                widget_key = json.loads(message['Key'])
            except KeyError:
                logging.error("No 'widget_key' found in the message body.")
            sqs.delete_message(QueueUrl=queue_url, ReceiptHandle=receipt_handle)
            return json.loads(message['Body']), widget_key
        else:
            logging.warning("No messages available in the queue.")
    except Exception as e:
        logging.error(f'Error reading message: {str(e)}')
    return None, None


def get_widget_s3(args):
    s3_client = boto3.client('s3')
    logging.info('Trying to get widget')
    widget_key = 'unknown'
    try:
        response = s3_client.list_objects_v2(Bucket=args['pull_bucket'])
        if 'Contents' in response:
            object_keys = [obj['Key'] for obj in response['Contents']]
            widget_key = min(object_keys)
            response = s3_client.get_object(Bucket=args['pull_bucket'], Key=widget_key)
            object_data = response['Body'].read()
            s3_client.delete_object(Bucket=args['pull_bucket'], Key=widget_key)
            if object_data:
                return json.loads(object_data.decode('utf-8')), widget_key
        raise
    except Exception as e:
        logging.error(f'Bad Reading: {widget_key}')
    return None, None


def get_args():
    parser = argparse.ArgumentParser(description='Pulls data from a AWS S3 bucket and pushes it to another bucket or DynamoDB table')
    parser.add_argument('--pull-choice', type=str, default='s3', help='Select pull choice (default=s3)')
    parser.add_argument('--push-choice', type=str, default='s3', help='Select push choice (default=s3)')
    parser.add_argument('-r', '--region', type=str, default='us-east-1', help='Name of AWS region used (default=us-east-1)')
    parser.add_argument('--pull-bucket', type=str, default='usu-cs5260-nate-requests', help='Name of bucket that will contain requests (default=usu-cs5260-nate-requests)')
    parser.add_argument('--pull-queue', type=str, default='cs5260-requests', help='Name of queue that will contain requests (default=cs5260-requests)')
    parser.add_argument('--push-bucket', type=str, default='usu-cs5260-nate-web', help='Name of bucket where widget info will be pushed (default=usu-cs5260-nate-web)')
    parser.add_argument('--push-table', type=str, default='widgets', help='Name of table where widget info will be pushed (default=widgets)')
    parser.add_argument('-mrt', '--max-runtime', type=int, default=1000, help='Maximum runtime in seconds (default=1000)')
    parser.add_argument('-mwr', '--max-widget-pulls', type=int, default=500, help='Maximum number of widget requests to pull (default=500)')
    parser.add_argument('-ipd', '--inter-pull-delay', type=int, default=100, help='Number of milliseconds to wait between request pulls (default=100)')
    parser.add_argument('-ll', '--log-level', type=bool, default=False, help='Level of logging (default=low)')
    return vars(parser.parse_args())


if __name__ == '__main__':
    main()
