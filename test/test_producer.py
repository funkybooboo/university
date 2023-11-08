import json
import unittest
from unittest.mock import MagicMock, patch
from src.producer import lambda_handler

class TestProducer(unittest.TestCase):

    @patch('src.producer.Session')
    def test_valid_lambda_handler(self, session_mock):
        event = {
            'body': json.dumps({"type": "create", "requestId": "e80fab52-71a5-4a76-8c4d-11b66b83ca2a",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": "Mary Matthews",
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI",
                                "otherAttributes": [{"name": "width-unit", "value": "cm"},
                                                    {"name": "length-unit", "value": "cm"},
                                                    {"name": "rating", "value": "2.580677"}, {"name": "note",
                                                                                              "value": "FEGYXHIJCTYNUMNMGZBEIDLKXYFNHFLVDYZRNWUDQAKQSVFLPRJTTXARVEIFDOLTUSWZZWVERNWPPOEYSUFAKKAPAGUALGXNDOVPNKQQKYWWOUHGOJWKAJGUXXBXLWAKJCIVPJYRMRWMHRUVBGVILZRMESQQJRBLXISNFCXGGUFZCLYAVLRFMJFLTBOTLKQRLWXALLBINWALJEMUVPNJWWRWLTRIBIDEARTCSLZEDLZRCJGSMKUOZQUWDGLIVILTCXLFIJIULXIFGRCANQPITKQYAKTPBUJAMGYLSXMLVIOROSBSXTTRULFYPDFJSFOMCUGDOZCKEUIUMKMMIRKUEOMVLYJNJQSMVNRTNGH"}]
                                })
        }
        mock_context = MagicMock()
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, {
        'statusCode': 200,
        'body': {'message': 'success widget push sqs'}
        })


    @patch('src.producer.Session')
    def test_invalid_type_lambda_handler(self, session_mock):
        event = {
            'body': json.dumps({"type": "test", "requestId": "e80fab52-71a5-4a76-8c4d-11b66b83ca2a",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": "Mary Matthews",
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI",
                                "otherAttributes": [{"name": "width-unit", "value": "cm"},
                                                    {"name": "length-unit", "value": "cm"},
                                                    {"name": "rating", "value": "2.580677"}, {"name": "note",
                                                                                              "value": "FEGYXHIJCTYNUMNMGZBEIDLKXYFNHFLVDYZRNWUDQAKQSVFLPRJTTXARVEIFDOLTUSWZZWVERNWPPOEYSUFAKKAPAGUALGXNDOVPNKQQKYWWOUHGOJWKAJGUXXBXLWAKJCIVPJYRMRWMHRUVBGVILZRMESQQJRBLXISNFCXGGUFZCLYAVLRFMJFLTBOTLKQRLWXALLBINWALJEMUVPNJWWRWLTRIBIDEARTCSLZEDLZRCJGSMKUOZQUWDGLIVILTCXLFIJIULXIFGRCANQPITKQYAKTPBUJAMGYLSXMLVIOROSBSXTTRULFYPDFJSFOMCUGDOZCKEUIUMKMMIRKUEOMVLYJNJQSMVNRTNGH"}]
                                })
        }
        mock_context = MagicMock()
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, {
        'statusCode': 400,
        'body': {'error': "'invalid widget type'"}
        })

    @patch('src.producer.Session')
    def test_invalid_field1_lambda_handler(self, session_mock):
        event = {
            'body': json.dumps({"type": "create", "requestId": "e80fab52-71a5-4a76-8c4d-11b66b83ca2a",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": "Mary Matthews",
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI",
                                })
        }
        mock_context = MagicMock()
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, {
            'statusCode': 400,
            'body': {'error': "'missing required field: otherAttributes'"}
        })

    @patch('src.producer.Session')
    def test_invalid_field2_lambda_handler(self, session_mock):
        event = {
            'body': json.dumps({"type": "create", "requestId": "e80fab52-71a5-4a76-8c4d-11b66b83ca2a",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": 1234,
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI",
                                "otherAttributes": [{"name": "width-unit", "value": "cm"},
                                                    {"name": "length-unit", "value": "cm"},
                                                    {"name": "rating", "value": "2.580677"}, {"name": "note",
                                                                                              "value": "FEGYXHIJCTYNUMNMGZBEIDLKXYFNHFLVDYZRNWUDQAKQSVFLPRJTTXARVEIFDOLTUSWZZWVERNWPPOEYSUFAKKAPAGUALGXNDOVPNKQQKYWWOUHGOJWKAJGUXXBXLWAKJCIVPJYRMRWMHRUVBGVILZRMESQQJRBLXISNFCXGGUFZCLYAVLRFMJFLTBOTLKQRLWXALLBINWALJEMUVPNJWWRWLTRIBIDEARTCSLZEDLZRCJGSMKUOZQUWDGLIVILTCXLFIJIULXIFGRCANQPITKQYAKTPBUJAMGYLSXMLVIOROSBSXTTRULFYPDFJSFOMCUGDOZCKEUIUMKMMIRKUEOMVLYJNJQSMVNRTNGH"}]
                                })
        }
        mock_context = MagicMock()
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, {
            'statusCode': 400,
            'body': {'error': "'missing required field: owner'"}
        })


if __name__ == '__main__':
    unittest.main()
