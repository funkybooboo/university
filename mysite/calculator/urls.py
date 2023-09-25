from django.urls import path

from . import views

app_name = 'calculator'
urlpatterns = [
    path('', views.calculator, name='calculator'),
    path('calculator/', views.calculator, name='calculator'),
    path('recent_expressions/', views.recent_expressions, name='recent_expressions'),
    path('undefined_expressions/', views.undefined_expressions, name='undefined_expressions'),
    path('disagreeing_results/', views.disagreeing_results, name='disagreeing_results'),
    path('expressions_by_operator/', views.expressions_by_operator, name='expressions_by_operator'),
    path('plan/', views.plan, name='plan'),
    path('record/', views.record, name='record'),
    path('removeRecord/<int:expression_id>/', views.removeRecord, name='removeRecord'),
    path('error/', views.error, name='error')
]
