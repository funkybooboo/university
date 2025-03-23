from django.shortcuts import render

from .models import Units
from django.http import JsonResponse

def info(request):
    return render(request, 'unitconv/info.html')

def convert(request):
    try:
        fromUnit = str(request.GET.get('from', ''))
        value = float(request.GET.get('value', ''))
        toUnit = str(request.GET.get('to', ''))
        if (fromUnit == '' or value == '' or toUnit == ''):
            raise Exception
    except:
        return JsonResponse({'error': 'Invalid unit conversion request'})

    if not fromUnit == toUnit:
        unitList = []
        if Units.objects.order_by("-id").exists():
            unitList = Units.objects.order_by("-id")
        else:
            return JsonResponse({'error': 'Database Empty'})

        fromUnitList = []
        for unit in unitList:
            if not unit.from_unit in fromUnitList:
                fromUnitList.append(unit.from_unit)

        toUnitList = []
        for unit in unitList:
            if not unit.to_unit in toUnitList:
                toUnitList.append(unit.to_unit)

        if not fromUnit in fromUnitList or not toUnit in toUnitList:
            return JsonResponse({'error': 'Database does not have requested unit(s)'})

        result = 0.0
        resultUnit = ''
        for unit in unitList:
            if unit.from_unit == fromUnit and unit.to_unit == toUnit:
                resultUnit = unit.to_unit
                result = float(value * unit.to_value)
                break
        return JsonResponse({'units': resultUnit, 'value': result})
    else:
        return JsonResponse({'units': toUnit, 'value': value})
