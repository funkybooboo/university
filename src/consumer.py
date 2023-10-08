import sys
import boto3

REQUEST_BUCKET_NAME = 'usu-cs5260-nate-requests'

def main(args):
    if len(args) == 1:
        usage()
    storage_choice = args[1].lower()
    if not storage_choice == 'dynamodb' or not storage_choice == 's3':
        usage()

    s3_client = boto3.client('s3')



def usage():
    print('Please provide a storage option: dynamodb or s3')
    print()
    print('Usage examples:')
    print('\t$ python consumer.py s3')
    print('\t$ python consumer.py dynamodb')
    sys.exit(1)

if __name__ == '__main__':
    main(sys.argv)