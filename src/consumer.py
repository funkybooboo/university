import sys
import boto3
import time
import json
import logging

REGION_NAME = 'us-east-1'  # US East (N. Virgina)
REQUEST_S3_BUCKET_NAME = 'usu-cs5260-nate-requests'
STORAGE_S3_BUCKET_NAME = 'usu-cs5260-nate-web'
STORAGE_DYNAMODB_NAME = 'widgets'


def main(args):
    storage_choice = process_args(args)
    if storage_choice is None:
        return
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
    missed_count = 0
    wait_time_ms = 100 / 1000
    try:
        while missed_count < 500:
            widget, widget_key = get_widget(s3_client)
            if widget is None:
                logging.info('Did not find a widget')
                missed_count += 1
                time.sleep(wait_time_ms)
                continue
            logging.info(f'Got widget: {widget_key}')
            missed_count = 0
            process_widget(widget, widget_key, storage_choice, s3_client)
    except Exception as e:
        logging.error('An error occurred:', exc_info=True)
    finally:
        logging.info('End Program')
        logging.shutdown()


def process_widget(widget, widget_key, storage_choice, s3_client):
    try:
        if widget['type'] == 'create' and is_valid(widget):
            create_widget(storage_choice, widget, s3_client)
        else:
            raise
    except Exception as e:
        logging.error(f'Bad Processing: {widget_key}')


def create_widget(storage_choice, widget, s3_client):
    logging.info('processing valid widget')
    if storage_choice == 's3':
        put_s3_object(widget, s3_client)
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


def put_s3_object(widget, s3_client):
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    item_content = str(widget)
    s3_client.put_object(Bucket=STORAGE_S3_BUCKET_NAME, Key=object_key, Body=item_content)
    logging.info('successful put to s3')


def is_valid(widget):
    return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(widget['description']) == str


def get_widget(s3_client):
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


def process_args(args):
    if len(args) == 1:
        return 's3'
    if args[1] == '-h' or args[1] == '--help':
        usage()
    elif args[1] == 's3':
        return 's3'
    elif args[1] == 'dynamodb':
        return 'dynamodb'
    else:
        print('Invalid option(s)')
        print('For help: python consumer.py --help')
        return None


def usage():
    print('Author: Nate Stott Utah State University Computer Science Student')
    print('Date: Oct 10, 2023')
    print('Project made for CS5260 Cloud Computing with Steve Petruzza')
    print('''
Command-line arguments (both short and long styles):
    -h, --help                     Display help message

Examples:
    python consumer.py                     Default is s3
    python consumer.py --help
    python consumer.py s3
    python consumer.py dynamodb

Note: You must have full access to the request bucket, and storage dynamodb table and s3 bucket.
    ''')

if __name__ == '__main__':
    main(sys.argv)
