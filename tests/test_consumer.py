import unittest
from unittest.mock import patch, MagicMock
import src.consumer as consumer


class TestConsumerFunctions(unittest.TestCase):

    @patch('boto3.resource')
    def test_put_dynamodb_object(self, mock_boto3_resource):
        widget = {'widgetId': 'test-id', 'owner': 'test-owner', 'label': 'test-label', 'description': 'test-des', 'otherAttributes': []}
        args = {'push_table': 'widgets', 'region': 'us-east-1'}
        consumer.create_dynamodb_object(widget, args)

    def test_put_s3_object(self):
        mock_s3_client = MagicMock()
        widget = {'owner': 'Nate Stott', 'widgetId': 'test'}
        args = {'push_bucket': 'test'}
        consumer.create_s3_object(widget, mock_s3_client, args)
        mock_s3_client.put_object.assert_called_once()

    def test_get_widget(self):
        mock_s3_client = MagicMock()
        mock_s3_client.list_objects_v2.return_value = {'Contents': [{'Key': '1'}, {'Key': '2'}, {'Key': '3'}]}
        mock_response_stream = MagicMock()
        mock_response_stream.read.return_value = b'{}'
        mock_s3_client.get_object.return_value = {'Body': mock_response_stream}
        args = {'pull_bucket': 'test'}
        data, key = consumer.get_widget_s3(mock_s3_client, args)
        self.assertEquals(data, {})
        self.assertEquals(key, '1')


if __name__ == '__main__':
    unittest.main()
