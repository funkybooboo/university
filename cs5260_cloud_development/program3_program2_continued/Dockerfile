FROM python:latest

COPY ./src/consumer.py .

RUN pip install --no-cache-dir boto3

CMD ["python", "./consumer.py"]