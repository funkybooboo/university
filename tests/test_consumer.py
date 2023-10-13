import unittest
from unittest.mock import patch, Mock
import src.consumer as consumer


class TestConsumerFunctions(unittest.TestCase):

    @patch('src.consumer.boto3.client')
    @patch('src.consumer.s3_client.list_objects_v2', return_value={'Contents': []})
    @patch('src.consumer.s3_client.get_object')
    @patch('src.consumer.s3_client.delete_object')
    def test_get_widget(self, mock_delete_object, mock_get_object, mock_list_objects_v2, mock_boto3_client):
        mock_response = Mock()
        mock_response.read.return_value = b'{}'
        mock_get_object.return_value = mock_response

        args = {
            'pull_bucket': 'test-bucket',
        }

        widget, widget_key = consumer.get_widget(mock_boto3_client, args)

        mock_list_objects_v2.assert_called_with(Bucket='test-bucket')
        mock_get_object.assert_called()
        mock_delete_object.assert_called()

    @patch('src.consumer.s3_client.list_objects_v2', return_value={'Contents': []})
    @patch('src.consumer.s3_client.get_object')
    @patch('src.consumer.s3_client.delete_object')
    def test_get_smallest_object_data(self, mock_delete_object, mock_get_object, mock_list_objects_v2):
        mock_response = Mock()
        mock_response.read.return_value = b'{}'
        mock_get_object.return_value = mock_response

        args = {
            'pull_bucket': 'test-bucket',
        }

        object_data, widget_key = consumer.get_smallest_object_data(args, mock_list_objects_v2, args)

        mock_list_objects_v2.assert_called_with(Bucket='test-bucket')
        mock_get_object.assert_called()
        mock_delete_object.assert_called()

    @patch('src.consumer.boto3.client')
    def test_put_s3_object(self, mock_boto3_client):
        widget = {
            'type': 'create',
            'widgetId': 'test-widget-id',
            'owner': 'Test Owner',
            'label': 'Test Label',
            'description': 'Test Description',
            'otherAttributes': [],
        }
        args = {
            'push_bucket': 'test-bucket',
        }

        consumer.put_s3_object(widget, mock_boto3_client, args)

        mock_boto3_client.return_value.put_object.assert_called_with(
            Bucket='test-bucket',
            Key='widgets/test-owner/test-widget-id',
            Body=str(widget),
        )

    @patch('src.consumer.boto3.resource')
    def test_put_dynamodb_object(self, mock_boto3_resource):
        widget = {
            'widgetId': 'test-widget-id',
            'owner': 'Test Owner',
            'label': 'Test Label',
            'description': 'Test Description',
            'otherAttributes': [],
        }
        args = {
            'region': 'us-west-2',
            'push_table': 'test-table',
        }

        consumer.put_dynamodb_object(widget, args)

        mock_table = mock_boto3_resource.return_value.Table.return_value
        mock_table.put_item.assert_called_with(
            Item={
                'id': 'test-widget-id',
                'owner': 'Test Owner',
                'label': 'Test Label',
                'description': 'Test Description',
            }
        )

    def test_is_valid_valid_widget(self):
        widget = {
            'widgetId': 'test-widget-id',
            'owner': 'Test Owner',
            'label': 'Test Label',
            'description': 'Test Description',
        }

        result = consumer.is_valid(widget)

        self.assertTrue(result)

    def test_is_valid_invalid_widget(self):
        widget = {
            'widgetId': 'test-widget-id',
            'owner': 'Test Owner',
            'label': 'Test Label',
        }

        result = consumer.is_valid(widget)

        self.assertFalse(result)


if __name__ == '__main__':
    unittest.main()
