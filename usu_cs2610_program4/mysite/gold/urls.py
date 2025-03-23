from django.urls import path

from . import views

app_name = 'gold'
urlpatterns = [
    path('', views.gold, name='convert'),
    path('gold/', views.gold, name='convert'),
    path('plan/', views.plan, name='plan')
]
