from django.db import models

class Operator(models.Model):
    symbol = models.CharField(max_length=2)
    name = models.CharField(max_length=100)

class Expression(models.Model):
    num1 = models.FloatField()
    operator = models.ForeignKey(Operator, on_delete=models.RESTRICT)
    num2 = models.FloatField()
    defined = models.BooleanField()
    agree = models.BooleanField()
    PYAnswer = models.FloatField()
    JSAnswer = models.FloatField()
