import unittest
from unittest.mock import patch, MagicMock
from src.consumer import Session, create_s3_object, create_dynamodb_object, update_s3_object, update_dynamodb_object, delete_s3_object, delete_dynamodb_object, pull_widget_s3, pull_widget_sqs


class TestConsumerFunctions(unittest.TestCase):

    def test_create_s3_object(self):
        session_mock = MagicMock()
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'label': 'test_label',
            'description': 'hello',
            'otherAttributes': []
        }
        widget_key = 'test'
        create_s3_object(widget, widget_key, session_mock)

    @patch('boto3.Session')
    def test_create_dynamodb_object(self, mock):
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'label': 'test_label',
            'description': 'hello'
        }
        widget_key = 'test'
        create_dynamodb_object(widget, widget_key)

    @patch('boto3.Session')
    def test_update_s3_object(self, mock):
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'description': 'hello'
        }
        widget_key = 'test'
        update_s3_object(widget, widget_key)

    @patch('boto3.Session')
    def test_update_dynamodb_object(self, mock):
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'description': 'hello'
        }
        widget_key = 'test'
        update_dynamodb_object(widget, widget_key)

    @patch('boto3.Session')
    def test_delete_s3_object(self, mock):
        widget = {
            'widgetId': '1234',
            'owner': 'test',
        }
        widget_key = 'test'
        delete_s3_object(widget, widget_key)

    @patch('boto3.Session')
    def test_delete_dynamodb_object(self, mock):
        widget = {
            'widgetId': '1234',
            'owner': 'test',
        }
        widget_key = 'test'
        delete_dynamodb_object(widget, widget_key)

    @patch('boto3.Session')
    def test_pull_widget_s3(self, mock):
        widget, widget_key = pull_widget_s3()

    @patch('boto3.Session')
    def test_pull_widget_sqs(self, mock):
        widget, widget_key = pull_widget_sqs()


if __name__ == '__main__':
    unittest.main()
