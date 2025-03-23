from django.urls import path
from . import views

app_name = 'blog'
urlpatterns = [
    path('', views.home, name='home'),
    path('home/', views.home, name='home'),
    path('archive/', views.archive, name='archive'),
    path('entry/<int:blog_id>/', views.entry, name='entry'),
    path('blogPost/', views.blogPost, name='blogPost'),
    path('about/', views.about, name='about'),
    path('plan/', views.plan, name='plan'),
    path('techtips+css/', views.techtipsWithCss, name='techtips+css'),
    path('techtips-css/', views.techtipsNoCss, name='techtips-css'),
    path('makeComment/<int:blog_id>/', views.makeComment, name='makeComment'),
    path('makeBlogPost', views.makeBlogPost, name='makeBlogPost')
]
