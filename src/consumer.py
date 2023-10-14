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
    s3_client = boto3.client('s3')
    logging.info('Got s3 client')
    consume(s3_client, args)


def consume(s3_client, args):
    end = (time.time() * 1000) + args['max_runtime']
    count = 0
    try:
        while (time.time() * 1000) < end and count < args['max_widget_pulls']:
            widget, widget_key = get_widget(s3_client, args)
            if widget is None:
                logging.info('Did not find a widget')
                time.sleep((args['inter_pull_delay'] / 1000))
                continue
            count += 1
            logging.info(f'Got widget: {widget_key}')
            process_widget(widget, widget_key, s3_client, args)
    except Exception as e:
        logging.error('An error occurred:', exc_info=True)
    finally:
        logging.info('End Program')
        logging.shutdown()


def process_widget(widget, widget_key, s3_client, args):
    try:
        if widget['type'] == 'create' and is_valid(widget):
            create_widget(widget, s3_client, args)
        else:
            raise
    except Exception as e:
        logging.error(f'Bad Processing: {widget_key}')


def create_widget(widget, s3_client, args):
    logging.info('processing valid widget')
    if args['storage_choice'] == 's3':
        put_s3_object(widget, s3_client, args)
    else:
        put_dynamodb_object(widget, args)


def put_dynamodb_object(widget, args):
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


def put_s3_object(widget, s3_client, args):
    widget_owner = widget['owner'].lower().replace(' ', '-')
    widget_id = widget['widgetId']
    object_key = f'widgets/{widget_owner}/{widget_id}'
    item_content = str(widget)
    s3_client.put_object(Bucket=args['push_bucket'], Key=object_key, Body=item_content)
    logging.info('successful put to s3')


def is_valid(widget):
    return type(widget['widgetId']) == str and type(widget['owner']) == str and type(widget['label']) == str and type(widget['description']) == str


def get_widget(s3_client, args):
    logging.info('Trying to get widget')
    widget_key = 'unknown'
    try:
        response = s3_client.list_objects_v2(Bucket=args['pull_bucket'])
        if 'Contents' in response:
            object_data, widget_key = get_smallest_object_data(args, response, s3_client)
            if object_data:
                return json.loads(object_data.decode('utf-8')), widget_key
        raise
    except Exception as e:
        logging.error(f'Bad Reading: {widget_key}')
    return None, None


def get_smallest_object_data(args, response, s3_client):
    object_keys = [obj['Key'] for obj in response['Contents']]
    widget_key = min(object_keys)
    response = s3_client.get_object(Bucket=args['pull_bucket'], Key=widget_key)
    object_data = response['Body'].read()
    s3_client.delete_object(Bucket=args['pull_bucket'], Key=widget_key)
    return object_data, widget_key


def get_args():
    parser = argparse.ArgumentParser(description='Pulls data from a AWS S3 bucket and pushes it to another bucket or DynamoDB table')
    parser.add_argument('-sc', '--storage-choice', type=str, default='s3', help='Select storage choice (default=s3)')
    parser.add_argument('-r', '--region', type=str, default='us-east-1', help='Name of AWS region used (default=us-east-1)')
    parser.add_argument('--pull-bucket', type=str, default='usu-cs5260-nate-requests', help='Name of bucket that will contain requests (default=usu-cs5260-nate-requests)')
    parser.add_argument('--push-bucket', type=str, default='usu-cs5260-nate-web', help='Name of bucket where widget info will be pushed (default=usu-cs5260-nate-web)')
    parser.add_argument('--push-table', type=str, default='widgets', help='Name of table where widget info will be pushed (default=widgets)')
    parser.add_argument('-mrt', '--max-runtime', type=int, default=30000, help='Maximum runtime in milliseconds (default=30000)')
    parser.add_argument('-mwr', '--max-widget-pulls', type=int, default=1000, help='Maximum number of widget requests to pull (default=1000)')
    parser.add_argument('-ipd', '--inter-pull-delay', type=int, default=100, help='Number of milliseconds to wait between request pulls (default=100)')
    parser.add_argument('-ll', '--log-level', type=bool, default=False, help='Level of logging (default=low)')
    args = parser.parse_args()
    return vars(args)


if __name__ == '__main__':
    main()
