from django.db import models

class Blog(models.Model):
    title_text = models.CharField(max_length=100)
    author_text = models.CharField(max_length=100)
    content_text = models.TextField(max_length=1000)
    posted_date = models.DateTimeField('date published')

class Comment(models.Model):
    blog_id = models.ForeignKey(Blog, on_delete=models.CASCADE)
    commenter_text = models.CharField(max_length=200)
    email_text = models.EmailField(max_length=100)
    content_text = models.TextField(max_length=1000)
    posted_date = models.DateTimeField('date published')
