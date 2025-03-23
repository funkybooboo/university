from django.urls import reverse
from django.utils import timezone
from django.http import HttpResponseRedirect
from django.shortcuts import render
from .models import Blog, Comment

def home(request):
    latest_blog_list = Blog.objects.order_by('-posted_date')[:3]
    comment_list = Comment.objects.order_by('-blog_id')
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    blogCommentCount = {}
    for blog in latest_blog_list:
        count = 0
        for comment in comment_list:
            if blog.id == comment.blog_id_id:
                count += 1
        blogCommentCount.update({blog.id : count})
    context = {
        'blogCommentCount' : blogCommentCount,
        'latest_blog_list' : latest_blog_list,
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/home.html', context)

def blogPost(request):
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    context = {
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/postblog.html', context)

def makeBlogPost(request):
    blog = Blog()
    try:
        blog.title_text = request.POST.get('title_text')
        blog.author_text = request.POST.get('author_text')
        blog.content_text = request.POST.get('content_text')
        blog.posted_date = timezone.now()
        if blog.title_text == '' or blog.author_text == '' or blog.content_text == '':
            raise Exception
    except:
        context = {
            'error_message' : 'Please fill in all of the fields'
        }
        return render(request, 'blog/error.html', context)
    else:
        blog.save(request)
        return HttpResponseRedirect(reverse('blog:home'))

def makeComment(request, blog_id):
    comment = Comment()
    try:
        comment.blog_id_id = blog_id
        comment.commenter_text = request.POST.get('commenter_text')
        comment.email_text = request.POST.get('email_text')
        comment.content_text = request.POST.get('content_text')
        comment.posted_date = timezone.now()
        if comment.commenter_text == '' or comment.content_text == '':
            raise Exception
    except:
        context = {
            'error_message' : 'Please fill in all of the fields'
        }
        return render(request, 'blog/error.html', context)
    else:
        comment.save(request)
        return HttpResponseRedirect(reverse('blog:entry', args=(comment.blog_id_id,)))

def archive(request):
    latest_blog_list = Blog.objects.order_by('-posted_date')
    comment_list = Comment.objects.order_by('-blog_id')
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    blogCommentCount = {}
    for blog in latest_blog_list:
        count = 0
        for comment in comment_list:
            if blog.id == comment.blog_id_id:
                count += 1
        blogCommentCount.update({blog.id: count})
    context = {
        'blogCommentCount': blogCommentCount,
        'latest_blog_list' : latest_blog_list,
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/archive.html', context)

def entry(request, blog_id):
    try:
        blog = Blog.objects.get(pk=blog_id)
    except:
        blog = ''
    comment_list = Comment.objects.order_by('-blog_id')
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    if blog == '':
        context = {
            'count' : 0,
            'comment_list': comment_list,
            'blog': blog,
            'todays_date': todays_date,
            'users_ip': users_ip
        }
    else:
        todays_date = timezone.now()
        users_ip = get_client_ip(request)
        count = 0
        for comment in comment_list:
            if (comment.blog_id_id == blog_id):
                count += 1
        context = {
            'count' : count,
            'comment_list' : comment_list,
            'blog' : blog,
            'todays_date': todays_date,
            'users_ip': users_ip
        }
    return render(request, 'blog/entry.html', context)

def about(request):
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    context = {
        'todays_date' : todays_date,
        'users_ip' : users_ip
    }
    return render(request, 'blog/about.html', context)

# got from stack overflow https://stackoverflow.com/questions/4581789/how-do-i-get-user-ip-address-in-django
def get_client_ip(request):
    x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
    if x_forwarded_for:
        ip = x_forwarded_for.split(',')[0]
    else:
        ip = request.META.get('REMOTE_ADDR')
    return ip

def plan(request):
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    context = {
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/plan.html', context)

def techtipsWithCss(request):
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    context = {
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/techtips+css.html', context)

def techtipsNoCss(request):
    todays_date = timezone.now()
    users_ip = get_client_ip(request)
    context = {
        'todays_date': todays_date,
        'users_ip': users_ip
    }
    return render(request, 'blog/techtips-css.html', context)
