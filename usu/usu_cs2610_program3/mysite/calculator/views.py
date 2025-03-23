from django.http import HttpResponseRedirect
from django.shortcuts import render
from django.urls import reverse
from .models import Expression, Operator
import math

def calculator(request):
    lastID = 1
    latest_expression_list = []
    if Expression.objects.order_by("-id").exists():
        latest_expression_list = Expression.objects.order_by("-id")
        lastID = int(latest_expression_list[0].id) + 1
    print("lastID:"+str(lastID))
    context = {
        'lastID': lastID,
        'latest_expression_list': latest_expression_list[:5]
    }
    return render(request, 'calculator/calculator.html', context)

def recent_expressions(request):
    latest_expression_list = []
    if Expression.objects.order_by("-operator_id").exists():
        latest_expression_list = Expression.objects.order_by("-operator_id")
    context = {
        "latest_expression_list": latest_expression_list
    }
    return render(request, 'calculator/recent_expressions.html', context)

def undefined_expressions(request):
    latest_expression_list = []
    list = []
    if Expression.objects.order_by("-operator_id").exists():
        latest_expression_list = Expression.objects.order_by("-operator_id")
    for expression in latest_expression_list:
        if not expression.defined:
            list.append(expression)
    context = {
        "undefined_expression_list" : list
    }
    return render(request, 'calculator/undefined_expressions.html', context)

def disagreeing_results(request):
    latest_expression_list = []
    list = []
    if Expression.objects.order_by("-operator_id").exists():
        latest_expression_list = Expression.objects.order_by("-operator_id")
    for expression in latest_expression_list:
        if not expression.agree:
            list.append(expression)
    context = {
        "disagreeing_results_list" : list
    }
    return render(request, 'calculator/disagreeing_results.html', context)

def expressions_by_operator(request):
    latest_expression_list = []
    plusList = []
    minusList = []
    multiplyList = []
    divideList = []
    exponentList = []
    modulusList = []
    if Expression.objects.order_by("-operator_id").exists():
        latest_expression_list = Expression.objects.order_by("-operator_id")
    for expression in latest_expression_list:
        if expression.operator.name == "plus":
            plusList.append(expression)
        elif expression.operator.name == "minus":
            minusList.append(expression)
        elif expression.operator.name == "multiply":
            multiplyList.append(expression)
        elif expression.operator.name == "divide":
            divideList.append(expression)
        elif expression.operator.name == "exponent":
            exponentList.append(expression)
        elif expression.operator.name == "modulus":
            modulusList.append(expression)
    context = {
        "latest_expression_list" : latest_expression_list,
        "plusList" : plusList,
        "minusList" : minusList,
        "multiplyList" : multiplyList,
        "divideList" : divideList,
        "exponentList" : exponentList,
        "modulusList" : modulusList
    }
    return render(request, 'calculator/expressions_by_operator.html', context)

def plan(request):
    return render(request, 'calculator/plan.html')

def removeRecord(request, expression_id):
    expression = Expression.objects.get(pk=expression_id)
    expression.delete()
    cameFrom = str(request.POST.get("cameFrom"))
    if "HTTP_REFERER" in request.META:
        return HttpResponseRedirect(request.META["HTTP_REFERER"])
    elif cameFrom == "recent_expressions":
        return HttpResponseRedirect(reverse('calculator:recent_expressions'))
    elif cameFrom == "disagreeing_results":
        return HttpResponseRedirect(reverse('calculator:disagreeing_results'))
    elif cameFrom == "expressions_by_operator":
        return HttpResponseRedirect(reverse('calculator:expressions_by_operator'))
    elif cameFrom == "undefined_expressions":
        return HttpResponseRedirect(reverse('calculator:undefined_expressions'))
    else:
        return HttpResponseRedirect(reverse('calculator:calculator'))

def record(request):
    expression = Expression()

    try:
        expression.num1 = float(request.POST.get("num1"))
        symbol = str(request.POST.get("operator")).strip()
        expression.num2 = float(request.POST.get("num2"))
        expression.JSAnswer = request.POST.get("js_answer")
    except:
        return HttpResponseRedirect(reverse('calculator:error'))

    try:
        expression.JSAnswer = float(expression.JSAnswer)
        if math.isnan(expression.JSAnswer) or math.isinf(expression.JSAnswer):
            expression.JSAnswer = "undefined"
    except:
        expression.JSAnswer = "undefined"

    if str(symbol) == "/" and str(expression.num2) == "0":
        expression.PYAnswer = "undefined"
    else:
        expression.PYAnswer = eval(str(expression.num1) + str(symbol) + str(expression.num2))
        try:
            expression.PYAnswer = float(expression.PYAnswer)
            if math.isnan(expression.PYAnswer) or math.isinf(expression.PYAnswer):
                expression.PYAnswer = "undefined"
        except:
            expression.PYAnswer = "undefined"

    if str(expression.JSAnswer) == str(expression.PYAnswer):
        expression.agree = True
    else:
        expression.agree = False

    if symbol == "+":
        expression.operator = Operator.objects.get(name="plus")
    elif symbol == "-":
        expression.operator = Operator.objects.get(name="minus")
    elif symbol == "*":
        expression.operator = Operator.objects.get(name="multiply")
    elif symbol == "/":
        expression.operator = Operator.objects.get(name="divide")
    elif symbol == "%":
        expression.operator = Operator.objects.get(name="modulus")
    elif symbol == "**":
        expression.operator = Operator.objects.get(name="exponent")
    else:
        return HttpResponseRedirect(reverse('calculator:error'))

    if expression.PYAnswer == "undefined":
        expression.defined = False
    else:
        expression.defined = True

    expression.save(request)

    return HttpResponseRedirect(reverse('calculator:calculator'))

def error(request):
    return render(request, 'calculator/error.html')
