import unittest
from unittest.mock import MagicMock, patch
from src.producer import lambda_handler, validate_widget, validate_create, validate_update, validate_delete, push_widget_sqs

class TestProducer(unittest.TestCase):

    @patch('Session')
    def test_lambda_handler(self, mock_session):
        mock_event = MagicMock()
        mock_context = MagicMock()
        lambda_handler(mock_event, mock_context)


    def test_validate_widget(self):
        widget = {}
        validate_widget(widget)


    def test_validate_create(self):
        widget = {}
        validate_create(widget)


    def test_validate_update(self):
        widget = {}
        validate_update(widget)


    def test_validate_delete(self):
        widget = {}
        validate_delete(widget)


    def test_push_widget_sqs(self):
        widget = {}
        mock_session = MagicMock()
        push_widget_sqs(widget, mock_session)


if __name__ == '__main__':
    unittest.main()
