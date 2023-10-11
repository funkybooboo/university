import unittest
from src.consumer import (main, process_args, create_widget, put_dynamodb_object, put_s3_object, is_valid, get_widget, usage)


class MyTestCase(unittest.TestCase):
    def test_main(self):
        self.assertEqual(True, False)

    def test_get_storage_choice(self):
        pass

    def test_process_widget(self):
        pass

    def test_create_widget(self):
        pass

    def test_put_dynamodb_object(self):
        pass

    def test_put_s3_object(self):
        pass

    def test_is_valid(self):
        pass

    def test_get_widget(self):
        pass

    def test_usage(self):
        pass


if __name__ == '__main__':
    unittest.main()
