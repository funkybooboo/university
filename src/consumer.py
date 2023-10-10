import sys
import boto3
import time
import json
import logging

REGION_NAME = 'us-east-1'  # US East (N. Virgina)
REQUEST_S3_BUCKET_NAME = 'usu-cs5260-nate-requests'
STORAGE_S3_BUCKET_NAME = 'usu-cs5260-nate-web'
STORAGE_DYNAMODB_NAME = 'widgets'

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('consumer.log'),
        logging.StreamHandler()
    ]
)
logging.info('Start program')
s3_client = boto3.client('s3')
logging.info('Got s3 client')


def main(args):
    storage_choice = get_storage_choice(args)
    missed_count = 0
    wait_time_ms = 100 / 1000
    try:
        while missed_count < 500:
            widget, widget_key = get_widget()
            if widget is None:
                logging.info('Did not find a widget')
                missed_count += 1
                time.sleep(wait_time_ms)
                continue
            logging.info(f'Got widget: {widget_key}')
            missed_count = 0
            process_widget(widget, widget_key, storage_choice)
    except Exception as e:
        logging.error('End Program bad')
        logging.error('An error occurred:', exc_info=True)
    finally:
        logging.info('End Program good')
        logging.shutdown()


def get_storage_choice(args):
    if len(args) == 1:
        usage()
    storage_choice = args[1].lower()
    if not storage_choice == 'dynamodb' and not storage_choice == 's3':
        usage()
    return storage_choice


def process_widget(widget, widget_key, storage_choice):
    try:
        if widget['type'] == 'create' and is_valid(widget):
            create_widget(storage_choice, widget)
        else:
            raise
    except Exception as e:
        logging.error(f'Bad Processing: {widget_key}')


def create_widget(storage_choice, widget):
    logging.info('processing valid widget')
    if storage_choice == 's3':
        put_s3_object(widget)
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
    logging.info('successful put to dynamodb')


def put_s3_object(widget):
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    item_content = str(widget)
    s3_client.put_object(Bucket=STORAGE_S3_BUCKET_NAME, Key=object_key, Body=item_content)
    logging.info('successful put to s3')


def is_valid(widget):
    return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(widget['description']) == str


def get_widget():
    logging.info('Trying to get widget')
    widget_key = 'unknown'
    try:
        response = s3_client.list_objects_v2(Bucket=REQUEST_S3_BUCKET_NAME)
        if 'Contents' in response:
            object_keys = [obj['Key'] for obj in response['Contents']]
            widget_key = min(object_keys)
            response = s3_client.get_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=widget_key)
            object_data = response['Body'].read()
            s3_client.delete_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=widget_key)
            if object_data:
                return json.loads(object_data.decode('utf-8')), widget_key
            raise
    except Exception as e:
        logging.error(f'Bad Reading: {widget_key}')
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
