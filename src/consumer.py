import boto3
import time
import json
import logging
import argparse


def main():
    args = get_args()
    setup_logging(args)
    logging.info('start program execution')
    if args['pull_choice'] == 's3' and args['push_choice'] == 's3':
        consume(args, pull_widget_s3, push_widget_s3)
    elif args['pull_choice'] == 's3' and args['push_choice'] == 'dynamodb':
        consume(args, pull_widget_s3, push_widget_dynamodb)
    elif args['pull_choice'] == 'sqs' and args['push_choice'] == 's3':
        consume(args, pull_widget_sqs, push_widget_s3)
    elif args['pull_choice'] == 'sqs' and args['push_choice'] == 'dynamodb':
        consume(args, pull_widget_sqs, push_widget_dynamodb)
    else:
        logging.error('invalid push or pull options')
    logging.info('end program execution')
    logging.shutdown()


def setup_logging(args):
    log_level = logging.DEBUG if args['log_level'] else logging.INFO
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('consumer.log'),
            logging.StreamHandler()
        ]
    )


def consume(args, pull, push):
    end = (time.time() * 1000) + (args['max_runtime'] * 1000)
    count = 0
    while (time.time() * 1000) < end and count < args['max_widget_pulls']:
        widget, widget_key = pull(args)
        count += 1
        if widget is None:
            logging.info('no widget')
            time.sleep((args['inter_pull_delay'] / 1000))
            continue
        logging.info(f'widget: {widget_key}')
        push(widget, widget_key, args)


def push_widget_s3(widget, widget_key, args):
    push_widget(widget, widget_key, args, create_s3_object, update_s3_object, delete_s3_object)


def push_widget_dynamodb(widget, widget_key, args):
    push_widget(widget, widget_key, args, create_dynamodb_object, update_dynamodb_object, delete_dynamodb_object)


def push_widget(widget, widget_key, args, create_object, update_object, delete_object):
    if widget['type'] == 'create' and is_valid_create(widget):
        logging.info('processing valid create widget')
        create_object(widget, widget_key, args)
    elif widget['type'] == 'update' and is_valid_update(widget):
        logging.info('processing valid update widget')
        update_object(widget, widget_key, args)
    elif widget['type'] == 'delete' and is_valid_delete(widget):
        logging.info('processing valid delete widget')
        delete_object(widget, widget_key, args)
    else:
        logging.error(f'invalid widget type: {widget_key}')


def create_s3_object(widget, widget_key, args):
    logging.info('attempt create widget s3')
    try:
        s3_client = boto3.client('s3')
        object_key = get_s3_object_key(widget)
        item = {
            'id': widget['widgetId'],
            'owner': widget['owner'],
            'label': widget['label'],
            'description': widget['description']
        }
        for i in widget['otherAttributes']:
            item.update({i['name']: i['value']})
        s3_client.put_object(Bucket=args['push_bucket'], Key=object_key, Body=str(item))
        logging.info(f'success create s3: {widget_key}')
    except Exception as e:
        logging.error(f'fail create s3: {widget_key}')
        logging.error(e)


def create_dynamodb_object(widget, widget_key, args):
    logging.info('attempt create widget dynamodb')
    try:
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
        logging.info(f'success create dynamodb: {widget_key}')
    except Exception as e:
        logging.error(f'fail create dynamodb: {widget_key}')
        logging.error(e)


def update_s3_object(widget, widget_key, args):
    logging.info('attempt update widget s3')
    try:
        object_key = get_s3_object_key(widget)
        s3_client = boto3.client('s3')
        response = s3_client.get_object(Bucket=args['push_bucket'], Key=object_key)
        object_data = response['Body'].read()
        s3_client.delete_object(Bucket=args['push_bucket'], Key=object_key)
        if object_data:
            old_widget = json.loads(object_data.decode('utf-8').replace("'", '"'))
            item = update_item(old_widget, widget)
            s3_client.put_object(Bucket=args['push_bucket'], Key=object_key, Body=str(item))
            logging.info(f'success update widget s3: {widget_key}')
            return
        logging.error(f'fail update widget s3: {widget_key}')
    except Exception as e:
        logging.error(e)


def update_dynamodb_object(widget, widget_key, args):
    logging.info('attempt update widget dynamodb')
    try:
        dynamodb = boto3.resource('dynamodb', region_name=args['region'])
        table = dynamodb.Table(args['push_table'])
        key = {'id': {'S': widget['id']}}
        response = table.get_item(Key=key)
        if 'Item' in response:
            old_widget = response['Item']
            table.delete_item(Key=key)
            item = update_item(old_widget, widget)
            table.put_item(Item=item)
            logging.info(f'success update widget dynamodb: {widget_key}')
            return
        logging.error(f'fail to update widget dynamodb: {widget_key}')
    except Exception as e:
        logging.error(e)


def update_item(old_widget, widget):
    item = old_widget.copy()
    if widget['description']:
        item['description'] = widget['description']
    if widget['label']:
        item['label'] = widget['label']
    for i in widget['otherAttributes']:
        item.update({i['name']: i['value']})
    return item


def delete_s3_object(widget, widget_key, args):
    logging.info('attempt delete widget s3')
    try:
        object_key = get_s3_object_key(widget)
        s3_client = boto3.client('s3')
        s3_client.delete_object(Bucket=args['push_bucket'], Key=object_key)
        logging.info(f'success delete widget s3: {widget_key}')
    except Exception as e:
        logging.error(f'fail delete widget s3: {widget_key}')
        logging.error(e)


def delete_dynamodb_object(widget, widget_key, args):
    logging.info('attempt delete widget dynamodb')
    try:
        dynamodb = boto3.resource('dynamodb', region_name=args['region'])
        table = dynamodb.Table(args['push_table'])
        table.delete_item(Key=widget['widgetId'])
        logging.info(f'success delete widget dynamodb: {widget_key}')
    except Exception as e:
        logging.error(f'fail delete widget dynamodb: {widget_key}')
        logging.error(e)


def get_s3_object_key(widget):
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    return object_key


def is_valid_create(widget):
    try:
        return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(widget['description']) == str
    except Exception as e:
        logging.error(e)
        return False


def is_valid_update(widget):
    try:
        return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['description']) == str
    except Exception as e:
        logging.error(e)
        return False


def is_valid_delete(widget):
    try:
        return type(widget['widgetId']) == str and type(widget['owner']) == str
    except Exception as e:
        logging.error(e)
        return False


def pull_widget_sqs(args):
    logging.info('attempt widget pull sqs')
    widget_key = 'unknown'
    try:
        queue_url, sqs = get_queue_url(args)
        response = sqs.receive_message(
            QueueUrl=queue_url,
            MaxNumberOfMessages=1,
            WaitTimeSeconds=10
        )
        if 'Messages' in response:
            message = response['Messages'][0]
            receipt_handle = message['ReceiptHandle']
            try:
                widget_key = json.loads(message['Key'])
            except KeyError:
                logging.error("No 'widget_key' found in the message body.")
            sqs.delete_message(QueueUrl=queue_url, ReceiptHandle=receipt_handle)
            logging.info('success read got widget')
            return json.loads(message['Body']), widget_key
    except Exception as e:
        logging.error(f'fail read: {widget_key}')
        logging.error(e)
    logging.info('success read no widget')
    return None, None


def get_queue_url(args):
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
    return queue_url, sqs


def pull_widget_s3(args):
    logging.info('attempt widget pull s3')
    s3_client = boto3.client('s3')
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
                logging.info('success read got widget')
                return json.loads(object_data.decode('utf-8')), widget_key
    except Exception as e:
        logging.error(f'fail read: {widget_key}')
        logging.error(e)
    logging.info('success read no widget')
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
