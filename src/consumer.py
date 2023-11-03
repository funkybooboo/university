import boto3
import time
import json
import logging
import argparse


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
args = vars(parser.parse_args())
message_cache = []
widget_cache = []
s3_client = boto3.client('s3')
sts = boto3.client('sts')
sqs = boto3.client('sqs', region_name=args['region'])
dynamodb = boto3.resource('dynamodb', region_name=args['region'])
table = dynamodb.Table(args['push_table'])


def main():
    setup_logging()
    logging.info('start program execution')
    if args['pull_choice'] == 's3' and args['push_choice'] == 's3':
        consume(pull_widget_s3, push_widget_s3)
    elif args['pull_choice'] == 's3' and args['push_choice'] == 'dynamodb':
        consume(pull_widget_s3, push_widget_dynamodb)
    elif args['pull_choice'] == 'sqs' and args['push_choice'] == 's3':
        consume(pull_widget_sqs, push_widget_s3)
    elif args['pull_choice'] == 'sqs' and args['push_choice'] == 'dynamodb':
        consume(pull_widget_sqs, push_widget_dynamodb)
    else:
        logging.error('invalid push or pull options')
    logging.info('end program execution')
    logging.shutdown()


def setup_logging():
    log_level = logging.DEBUG if args['log_level'] else logging.INFO
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('consumer.log'),
            logging.StreamHandler()
        ]
    )


def consume(pull, push):
    end = (time.time() * 1000) + (args['max_runtime'] * 1000)
    count = 0
    while (time.time() * 1000) < end and count < args['max_widget_pulls']:
        widget, widget_key = pull()
        count += 1
        if widget is None:
            if len(widget_cache) > 0:
                logging.info('widget in cache')
                widget_info = widget_cache.pop(0)
                widget_key = widget_info[0]
                widget = widget_info[1]
            else:
                logging.info('no widget in cache')
                time.sleep((args['inter_pull_delay'] / 1000))
                continue
        if widget_key == 'unknown':
            widget_key = widget['requestId']
        logging.info(f'widget: {widget_key}')
        push(widget, widget_key)
    empty_widget_cache(push)


def empty_widget_cache(push):
    logging.info('main execution is done. emptying widget cache')
    widgets_left = widget_cache.copy()
    for widget_info in widgets_left:
        widget_cache.remove(widget_info)
        push(widget_info[1], widget_info[0])
    logging.info(f'couldn\'t handle {len(widget_cache)} widgets')
    for widget_info in widget_cache:
        logging.info(f'{widget_info[0]}')


def push_widget_s3(widget, widget_key):
    push_widget(widget, widget_key, create_s3_object, update_s3_object, delete_s3_object)


def push_widget_dynamodb(widget, widget_key):
    push_widget(widget, widget_key, create_dynamodb_object, update_dynamodb_object, delete_dynamodb_object)


def push_widget(widget, widget_key, create_object, update_object, delete_object):
    if widget['type'] == 'create' and is_valid_create(widget):
        logging.info('processing valid create widget')
        create_object(widget, widget_key)
    elif widget['type'] == 'update' and is_valid_update(widget):
        logging.info('processing valid update widget')
        update_object(widget, widget_key)
    elif widget['type'] == 'delete' and is_valid_delete(widget):
        logging.info('processing valid delete widget')
        delete_object(widget, widget_key)
    else:
        logging.error(f'invalid widget type: {widget_key}')


def create_s3_object(widget, widget_key):
    logging.info('attempt create widget s3')
    try:
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


def create_dynamodb_object(widget, widget_key):
    logging.info('attempt create widget dynamodb')
    try:
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


def update_s3_object(widget, widget_key):
    logging.info('attempt update widget s3')
    try:
        object_key = get_s3_object_key(widget)
        try:
            response = s3_client.get_object(Bucket=args['push_bucket'], Key=object_key)
        except Exception as e:
            logging.info(f'delay update widget s3: {widget_key}')
            widget_cache.append([widget_key, widget])
            return
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


def update_dynamodb_object(widget, widget_key):
    logging.info('attempt update widget dynamodb')
    try:
        key = {"id": widget["widgetId"]}
        response = table.get_item(Key=key)
        if 'Item' in response:
            old_widget = response['Item']
            table.delete_item(Key=key)
            item = update_item(old_widget, widget)
            try:
                table.put_item(Item=item)
            except Exception as e:
                logging.info(f'delay update widget dynamodb: {widget_key}')
                widget_cache.append([widget_key, widget])
                return
            logging.info(f'success update widget dynamodb: {widget_key}')
            return
        logging.error(f'fail to update widget dynamodb: {widget_key}')
    except Exception as e:
        logging.error(e)


def update_item(old_widget, widget):
    item = old_widget.copy()
    if 'description' in widget:
        item['description'] = widget['description']
    if 'label' in widget:
        item['label'] = widget['label']
    if 'otherAttributes' in widget:
        for i in widget['otherAttributes']:
            item.update({i['name']: i['value']})
    return item


def delete_s3_object(widget, widget_key):
    logging.info('attempt delete widget s3')
    try:
        object_key = get_s3_object_key(widget)
        try:
            s3_client.delete_object(Bucket=args['push_bucket'], Key=object_key)
        except Exception as e:
            logging.info(f'delay delete widget s3: {widget_key}')
            widget_cache.append([widget_key, widget])
            return
        logging.info(f'success delete widget s3: {widget_key}')
    except Exception as e:
        logging.error(f'fail delete widget s3: {widget_key}')
        logging.error(e)


def delete_dynamodb_object(widget, widget_key):
    logging.info('attempt delete widget dynamodb')
    try:
        key = {'id': widget['widgetId']}
        try:
            table.delete_item(Key=key)
        except Exception as e:
            logging.info(f'delay delete widget dynamodb: {widget_key}')
            widget_cache.append([widget_key, widget])
            return
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


def pull_widget_sqs():
    logging.info('attempt widget pull sqs')
    widget_key = 'unknown'
    try:
        queue_url = get_queue_url()
        if len(message_cache) == 0:
            response = sqs.receive_message(
                QueueUrl=queue_url,
                AttributeNames=['All'],
                MessageAttributeNames=['All'],
                MaxNumberOfMessages=10,
                WaitTimeSeconds=10
            )
            if 'Messages' in response:
                for message in response['Messages']:
                    receipt_handle = message['ReceiptHandle']
                    sqs.delete_message(QueueUrl=queue_url, ReceiptHandle=receipt_handle)
                    message_cache.append(message)
        if len(message_cache) > 0:
            logging.info('success read got widget')
            message = message_cache.pop(0)
            widget_key = message['MessageId']
            return json.loads(message['Body']), widget_key
        logging.info('success read no widget')
    except Exception as e:
        logging.error(f'fail read: {widget_key}')
        logging.error(e)
    return None, None


def get_queue_url():
    region = args['region']
    response = sts.get_caller_identity()
    account_number = response['Account']
    queue = args['pull_queue']
    queue_url = f'https://sqs.{region}.amazonaws.com/{account_number}/{queue}'
    return queue_url


def pull_widget_s3():
    logging.info('attempt widget pull s3')
    widget_key = 'unknown'
    try:
        response = s3_client.list_objects_v2(Bucket=args['pull_bucket'])
        if 'Contents' in response:
            widget_key = response['Contents'][0]
            response = s3_client.get_object(Bucket=args['pull_bucket'], Key=widget_key)
            object_data = response['Body'].read()
            s3_client.delete_object(Bucket=args['pull_bucket'], Key=widget_key)
            if object_data:
                logging.info('success read got widget')
                return json.loads(object_data.decode('utf-8')), widget_key
        else:
            logging.info('success read no widget')
    except Exception as e:
        logging.error(f'fail read: {widget_key}')
        logging.error(e)
    return None, None


if __name__ == '__main__':
    main()
