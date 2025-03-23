import unittest
from unittest.mock import MagicMock
from src.consumer import create_s3_object, create_dynamodb_object, update_s3_object, update_dynamodb_object, delete_s3_object, delete_dynamodb_object, pull_widget_s3, pull_widget_sqs


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
        session_mock.s3_client.put_object.assert_called_once()


    def test_create_dynamodb_object(self):
        session_mock = MagicMock()
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'label': 'test_label',
            'description': 'hello',
            'otherAttributes': []
        }
        widget_key = 'test'
        create_dynamodb_object(widget, widget_key, session_mock)
        session_mock.table.put_item.assert_called_once()


    def test_update_s3_object(self):
        session_mock = MagicMock()
        mock_response_stream = MagicMock()
        mock_response_stream.read.return_value = b'{\'widgetId\': \'1234\', \'owner\': \'test\' , \'description\': \'old description\', \'label\': \'old label\', \'otherAttributes\': []}'
        session_mock.s3_client.get_object.return_value = {'Body': mock_response_stream}
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'description': 'new description',
            'otherAttributes': []
        }
        widget_key = 'test'
        update_s3_object(widget, widget_key, session_mock)
        session_mock.s3_client.get_object.assert_called_once()
        session_mock.s3_client.delete_object.assert_called_once()
        session_mock.s3_client.put_object.assert_called_once()


    def test_update_dynamodb_object(self):
        session_mock = MagicMock()
        session_mock.table.get_item.return_value = {'Item': {'widgetId': '1234', 'owner': 'test' , 'description': 'old description', 'label': 'old label', 'otherAttributes': []}}
        widget = {
            'widgetId': '1234',
            'owner': 'test',
            'description': 'hello',
            'otherAttributes': []
        }
        widget_key = 'test'
        update_dynamodb_object(widget, widget_key, session_mock)
        session_mock.table.get_item.assert_called_once()
        session_mock.table.delete_item.assert_called_once()
        session_mock.table.put_item.assert_called_once()


    def test_delete_s3_object(self):
        session_mock = MagicMock()
        widget = {
            'widgetId': '1234',
            'owner': 'test',
        }
        widget_key = 'test'
        delete_s3_object(widget, widget_key, session_mock)
        session_mock.s3_client.delete_object.assert_called_once()



    def test_delete_dynamodb_object(self):
        session_mock = MagicMock()
        widget = {
            'widgetId': '1234',
            'owner': 'test',
        }
        widget_key = 'test'
        delete_dynamodb_object(widget, widget_key, session_mock)
        session_mock.table.delete_item.assert_called_once()


    def test_pull_widget_s3(self):
        session_mock = MagicMock()
        session_mock.s3_client.list_objects_v2.return_value = {'Contents': [{'Key': '1'}, {'Key': '2'}, {'Key': '3'}]}
        mock_response_stream = MagicMock()
        mock_response_stream.read.return_value = b'{}'
        session_mock.s3_client.get_object.return_value = {'Body': mock_response_stream}
        widget, widget_key = pull_widget_s3(session_mock)
        self.assertEquals(widget, {})
        self.assertEquals(widget_key, '1')
        session_mock.s3_client.list_objects_v2.assert_called_once()
        session_mock.s3_client.get_object.assert_called_once()
        session_mock.s3_client.delete_object.assert_called_once()



    def test_pull_widget_sqs(self):
        session_mock = MagicMock()
        session_mock.message_cache = []
        session_mock.sqs.receive_message.return_value = {'Messages' : [{'ReceiptHandle': MagicMock(), 'MessageId': '1', 'Body': '{}'}]}
        widget, widget_key = pull_widget_sqs(session_mock)
        self.assertEquals(widget, {})
        self.assertEquals(widget_key, '1')
        session_mock.sqs.receive_message.assert_called_once()
        session_mock.sqs.delete_message.assert_called_once()


if __name__ == '__main__':
    unittest.main()
