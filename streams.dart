List<int?>? flatten(List<List<int?>?>? list) =>
    list?.where((innerList) => innerList != null)
    .expand((innerList) => innerList!.where((element) => element != null))
    .toList();


List<dynamic>? deepen(List<int?>? list) {
  if (list == null) return null;
  if (list.isEmpty) return [];
  if (list.length == 1) return [list.firstOrNull];
  return [list.firstOrNull, deepen(list.sublist(1))];
}


Stream<int> Function() generateNumbers(int n) {
  return () async* {
    for (int i = 1; i <= n; i++) {
      await Future.delayed(Duration(seconds: 1));
      yield i;
    }
  };
}


Stream<int> Function() fibonnaciNumbers(int n) {
  return () async* {
    int a = 0, b = 1;
    for (int i = 0; i <= n; i++) {
      await Future.delayed(Duration(seconds: 1));
      yield a;
      final next = a + b;
      a = b;
      b = next;
    }
  };
}


Stream<int> Function() streamFilter(Stream<int> stream, bool Function(int) f) {
  return () async* {
    await for (final n in stream) {
      if (f(n)) yield n;
    }
  };
}


Stream<int> Function() streamAccumulation(Stream<int> stream, int Function(int, int) f, int initial) {
  return () async* {
    await for (final n in stream) {
      initial = f(n, initial);
      yield initial;
    }
  };
}


void main(List<String> arguments) async {
  print( 'flattening [[0,1], [2]] yields ${flatten([[0,1], [2]])}');
  print( 'flattening [[0,1], [2], null] yields ${flatten([[0,1], [2], null])}');
  print( 'flattening [[0,1, null], [2]] yields ${flatten([[0,1, null], [2]])}');
  print( 'flattening null yields ${flatten(null)}');
  print( 'flattening [null] yields ${flatten([null])}');
  print( 'deepening [0,1,2] yields ${deepen([0,1,2])}');
  print( 'deepening [0,null,2] yields ${deepen([0,null,2])}');
  print( 'deepening [0] yields ${deepen([0])}');
  print( 'deepening [] yields ${deepen([])}');
  print( 'deepening null yields ${deepen(null)}');
  await for (final number in fibonnaciNumbers(7)()) {
    print ('fibonnaci number is ${number}');
  }
  await for (final number in streamFilter(generateNumbers(10)(), ((a) =>  a % 2 == 0))()) {
    print ('filtered number is $number');
  }
  await for (final number in streamAccumulation(generateNumbers(10)(),((a,b) {return a+b;}),0)()) {
    print ('cumulative number is $number.');
  }
}



