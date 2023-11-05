import unittest
from unittest.mock import patch, MagicMock
from src.consumer import create_s3_object, create_dynamodb_object, update_s3_object, update_dynamodb_object, delete_s3_object, delete_dynamodb_object, pull_widget_s3, pull_widget_sqs


class TestConsumerFunctions(unittest.TestCase):

    def test_create_s3_object(self):

        create_s3_object()


    def test_create_dynamodb_object(self):

        create_dynamodb_object()


    def test_update_s3_object(self):

        update_s3_object()


    def test_update_dynamodb_object(self):

        update_dynamodb_object()


    def test_delete_s3_object(self):

        delete_s3_object()


    def test_delete_dynamodb_object(self):

        delete_dynamodb_object()


    def test_pull_widget_s3(self):

        pull_widget_s3()


    def test_pull_widget_sqs(self):

        pull_widget_sqs()


if __name__ == '__main__':
    unittest.main()
