from django.db import models

class Units(models.Model):

    from_value = models.FloatField()
    from_unit = models.CharField(max_length=10)
    from_unit_name = models.CharField(max_length=30)

    to_value = models.FloatField()
    to_unit = models.CharField(max_length=10)
    to_unit_name = models.CharField(max_length=30)