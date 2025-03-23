from django.shortcuts import render

from django.apps import apps

def gold(request):

    Units = apps.get_model('unitconv', 'Units')
    unitList = []
    if Units.objects.order_by("-id").exists():
        unitList = Units.objects.order_by("-id")

    class Pair:
        def __init__(self, data1, data2):
            self.data1 = data1
            self.data2 = data2

    pair_list = []
    name_list = []
    measure_list = []
    for unit in unitList:
        name = unit.from_unit_name
        measure = unit.from_unit
        if not name in name_list and not measure in measure_list:
            name_list.append(name)
            measure_list.append(measure)
            pair = Pair(name, measure)
            pair_list.append(pair)

    context = {
        'pair_list' : pair_list,
        'measure_list' : measure_list
    }

    return render(request, 'gold/gold.html', context)

def plan(request):
    return render(request, 'gold/plan.html')
