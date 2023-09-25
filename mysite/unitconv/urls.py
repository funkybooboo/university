from django.urls import path

from . import views

app_name = 'unitconv'
urlpatterns = [
    path('', views.info, name='info'),
    path('info/', views.info, name='info'),
    path('convert/', views.convert, name='convert')
]