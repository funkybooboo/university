import sys
import boto3
import time
import json

REQUEST_S3_BUCKET_NAME = 'usu-cs5260-nate-requests'
STORAGE_S3_BUCKET_NAME = 'usu-cs5260-nate-web'
STORAGE_DYNAMODB_NAME = 'widgets'

def main(args):
    # if len(args) == 1:
    #     usage()
    # storage_choice = args[1].lower()
    # if not storage_choice == 'dynamodb' and not storage_choice == 's3':
    #     usage()
    storage_choice = 's3'
    s3_client = boto3.client('s3')
    count = 0
    while count < 100:
        widget = checkForRequest(s3_client)
        if widget == None:
            print('none')
            count += 1
            time.sleep(500/1000)
            continue
        count = 0
        process(widget, s3_client, storage_choice)

def process(widget, s3_client, storage_choice):
    if storage_choice == 's3':
        widget_owner = widget['owner'].lower().replace(' ', '-')
        widget_id = widget['widgetId']
        object_key = f'widgets/{widget_owner}/{widget_id}/'
        item_content = str(widget)
        s3_client.put_object(Bucket=STORAGE_S3_BUCKET_NAME, Key=object_key, Body=item_content)
    else:
        dynamodb = boto3.resource('dynamodb')
        table = dynamodb.Table(STORAGE_DYNAMODB_NAME)
        item = {
            'type': widget['type'],
            'requestId': widget['requestId'],
            'widgetId': widget['widgetId'],
            'owner': widget['owner'],
            'label': widget['label'],
            'description': widget['description']
        }
        for i in widget['otherAttributes']:
            item.update({i['name']: i['value']})
        table.put_item(Item=item)

def checkForRequest(s3_client):
    response = s3_client.list_objects_v2(Bucket=REQUEST_S3_BUCKET_NAME)
    if 'Contents' in response:
        object_keys = [obj['Key'] for obj in response['Contents']]
        smallest_key = min(object_keys)
        response = s3_client.get_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=smallest_key)
        object_data = response['Body'].read()
        s3_client.delete_object(Bucket=REQUEST_S3_BUCKET_NAME, Key=smallest_key)
        return json.loads(object_data.decode('utf-8'))
    else:
        return None

def test(items):
    with open('test.txt', 'w') as t:
        for item in items:
            t.write(str(type(item)))
            t.write('\n')
            t.write(str(item))
            t.write('\n')
    sys.exit(0)

def usage():
    print('Please provide a storage option: dynamodb or s3')
    print()
    print('Usage examples:')
    print('\t$ python consumer.py s3')
    print('\t$ python consumer.py dynamodb')
    sys.exit(1)

if __name__ == '__main__':
    main(sys.argv)