import sys
import boto3
import time
import json

REGION_NAME = 'us-east-1'  # US East (N. Virgina)
REQUEST_S3_BUCKET_NAME = 'usu-cs5260-nate-requests'
STORAGE_S3_BUCKET_NAME = 'usu-cs5260-nate-web'
STORAGE_DYNAMODB_NAME = 'widgets'


def main(args):
    try:
        if len(args) == 1:
            usage()
        storage_choice = args[1].lower()
        if not storage_choice == 'dynamodb' and not storage_choice == 's3':
            usage()
        s3_client = boto3.client('s3')
        missed_count = 0
        wait_time_ms = 100 / 1000
        while missed_count < 1000:
            widget, widget_key = get_widget(s3_client)
            if widget is None:
                missed_count += 1
                time.sleep(wait_time_ms)
                continue
            missed_count = 0
            process_widget(widget, widget_key, s3_client, storage_choice)
    except Exception as e:
        print(e)


def process_widget(widget, widget_key, s3_client, storage_choice):
    try:
        if widget['type'] == 'create':
            create_widget(s3_client, storage_choice, widget)
        else:
            raise
    except Exception as e:
        log_error(widget_key, 'Bad Processing')


def create_widget(s3_client, storage_choice, widget):
    if not is_valid(widget):
        raise
    if storage_choice == 's3':
        put_s3_object(s3_client, widget)
    else:
        put_dynamodb_object(widget)


def put_dynamodb_object(widget):
    dynamodb = boto3.resource('dynamodb', region_name=REGION_NAME)
    table = dynamodb.Table(STORAGE_DYNAMODB_NAME)
    item = {
        'id': widget['widgetId'],
        'owner': widget['owner'],
        'label': widget['label'],
        'description': widget['description']
    }
    for i in widget['otherAttributes']:
        item.update({i['name']: i['value']})
    table.put_item(Item=item)


def put_s3_object(s3_client, widget):
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    item_content = str(widget)
    s3_client.put_object(Bucket=STORAGE_S3_BUCKET_NAME, Key=object_key, Body=item_content)


def is_valid(widget):
    return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(
        widget['description']) == str


def log_error(widget_key, label):
    with open('log.txt', 'a') as log:
        log.write(f'{label}: {widget_key}\n')


def get_widget(s3_client):
    widget_key = 'unknown'
    try:
        response = s3_client.list_objects_v2(Bucket=REQUEST_S3_BUCKET_NAME)
        if 'Contents' in response:
            object_keys = [obj['Key'] for obj in response['Contents']]
            widget_key = min(object_keys)
            response = s3_client.get_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=widget_key)
            object_data = response['Body'].read()
            s3_client.delete_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=widget_key)
            print(widget_key)
            if object_data:
                return json.loads(object_data.decode('utf-8')), widget_key
            raise
    except Exception as e:
        log_error(widget_key, 'Bad Reading')
    return None, None


def usage():
    print('Please provide a storage option: dynamodb or s3')
    print()
    print('Usage examples:')
    print('\t$ python consumer.py s3')
    print('\t$ python consumer.py dynamodb')
    sys.exit(1)


if __name__ == '__main__':
    main(sys.argv)
