import json
import unittest
from unittest.mock import MagicMock, patch
from src.producer import lambda_handler


class TestProducer(unittest.TestCase):

    @patch('src.producer.Session')
    def test_valid_create(self, session_mock):
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        event = {
            'body': json.dumps({"type": "create",
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
        mock_context.aws_request_id = '5678'
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 200, "body": {"message": "success widget push sqs"}}')

    @patch('src.producer.Session')
    def test_valid_update(self, session_mock):
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        event = {
            'body': json.dumps(
                {"type": "update", "widgetId": "0510d371-0191-43d9-81d5-5a163659ae6f", "owner": "Mary Matthews",
                 "description": "LJEQMYCCJRZDQFJETHKWYRMWTOPRQYEBWFWQHQUPVTGRXURPPKEYAPEEYAAV",
                 "otherAttributes": [{"name": "color", "value": "blue"}, {"name": "size", "value": "360"},
                                     {"name": "size-unit", "value": "cm"}, {"name": "height", "value": "920"},
                                     {"name": "width", "value": "103"}, {"name": "width-unit", "value": "cm"},
                                     {"name": "rating", "value": "3.6255655"}, {"name": "price", "value": "98.76"},
                                     {"name": "note",
                                      "value": "YZAVYFSDXEPIARJWLVFLBMSADGGIVUVSTFHUCKOSWFVKWODINGKECEZFKHVLGWFKYHYVQZVHTMROSVTXMSWQGUMHFORJIZYOXPNOZLFPMCFQPKNRWMWUBTTPOVYPZWODIGBDCKZTBBNICWTPRONOMCCBZUJAYCRHFOJLPKHHELORIRLPLZGUQQHPBTRYZCJKZHMDUSFTXXGFTQHALLTPFIESBIUTKNNBYNDYROAEYQUNUFMZDYDKMVTMRPJMSBPLQVYZNEYAEUXIAQOJEXTLISTXIEESSTZYQDSQKLNGSJYMVGDZSSNHLFAIJWEVTRNQAYQPEITGNZSLERCGJIEVCVMB"}]})
        }
        mock_context = MagicMock()
        mock_context.aws_request_id = '5678'
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 200, "body": {"message": "success widget push sqs"}}')

    @patch('src.producer.Session')
    def test_valid_delete(self, session_mock):
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        event = {
            'body': json.dumps(
                {"type": "delete", "widgetId": "6984abeb-5b24-42eb-93cc-3a5bef6b4b8a", "owner": "Mary Matthews"})
        }
        mock_context = MagicMock()
        mock_context.aws_request_id = '5678'
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 200, "body": {"message": "success widget push sqs"}}')

    @patch('src.producer.Session')
    def test_invalid_type(self, session_mock):
        event = {
            'body': json.dumps({"type": "test",
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
        mock_context.aws_request_id = '5678'
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 400, "body": {"error": "invalid widget type"}}')

    @patch('src.producer.Session')
    def test_invalid_field1(self, session_mock):
        event = {
            'body': json.dumps({"type": "create",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": "Mary Matthews",
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI"
                                })
        }
        mock_context = MagicMock()
        mock_context.aws_request_id = '5678'
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 400, "body": {"error": "missing required field: otherAttributes"}}')

    @patch('src.producer.Session')
    def test_invalid_field2(self, session_mock):
        event = {
            'body': json.dumps({"type": "create",
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
        mock_context.aws_request_id = '5678'
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        response = lambda_handler(event, mock_context)
        self.assertEquals(response,
                          '{"statusCode": 400, "body": {"error": "required field: owner. required type: <class \'str\'>. got type: <class \'int\'>"}}')

    @patch('src.producer.Session')
    def test_no_body(self, session_mock):
        session_mock.sqs.send_message.return_value = {'MessageId': '1234'}
        event = {"type": "create",
                                "widgetId": "8123f304-f23f-440b-a6d3-80e979fa4cd6", "owner": "Mary Matthews",
                                "label": "JWJYY",
                                "description": "THBRNVNQPYAWNHGRGUKIOWCKXIVNDLWOIQTADHVEVMUAJWDONEPUEAXDITDSHJTDLCMHHSESFXSDZJCBLGIKKPUYAWKQAQI",
                                "otherAttributes": [{"name": "width-unit", "value": "cm"},
                                                    {"name": "length-unit", "value": "cm"},
                                                    {"name": "rating", "value": "2.580677"}, {"name": "note",
                                                                                              "value": "FEGYXHIJCTYNUMNMGZBEIDLKXYFNHFLVDYZRNWUDQAKQSVFLPRJTTXARVEIFDOLTUSWZZWVERNWPPOEYSUFAKKAPAGUALGXNDOVPNKQQKYWWOUHGOJWKAJGUXXBXLWAKJCIVPJYRMRWMHRUVBGVILZRMESQQJRBLXISNFCXGGUFZCLYAVLRFMJFLTBOTLKQRLWXALLBINWALJEMUVPNJWWRWLTRIBIDEARTCSLZEDLZRCJGSMKUOZQUWDGLIVILTCXLFIJIULXIFGRCANQPITKQYAKTPBUJAMGYLSXMLVIOROSBSXTTRULFYPDFJSFOMCUGDOZCKEUIUMKMMIRKUEOMVLYJNJQSMVNRTNGH"}]
        }
        mock_context = MagicMock()
        mock_context.aws_request_id = '5678'
        response = lambda_handler(event, mock_context)
        self.assertEquals(response, '{"statusCode": 400, "body": {"error": "Widget is None"}}')


if __name__ == '__main__':
    unittest.main()
