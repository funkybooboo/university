import unittest
from unittest.mock import MagicMock
from src.producer import lambda_handler

class TestProducer(unittest.TestCase):
    def test_lambda_handler(self):
        mock_event = MagicMock()
        mock_context = MagicMock()
        lambda_handler(mock_event, mock_context)



if __name__ == '__main__':
    unittest.main()
