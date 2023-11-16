List<int?>? flatten(List<List<int?>?>? list) =>
    list?.where((innerList) => innerList != null) // get all the innerLists and filter for nulls
    .expand((innerList) => innerList!.where((element) => element != null)) // unpack the elements and filter for nulls
    .toList(); // return a list


List<dynamic>? deepen(List<int?>? list) {
  if (list == null) return null;
  if (list.isEmpty) return [];
  if (list.length == 1) return [list.firstOrNull]; // end of recursion
  return [list.firstOrNull, deepen(list.sublist(1))]; // recursion to build to the deep lists
}


Stream<int> Function() generateNumbers(int n) {
  return () async* {
    for (int i = 1; i <= n; i++) {
      await Future.delayed(Duration(seconds: 1));
      yield i; // push i out to the caller
    }
  };
}


Stream<int> Function() fibonnaciNumbers(int n) {
  return () async* {
    int a = 0, b = 1;
    for (int i = 0; i <= n; i++) {
      await Future.delayed(Duration(seconds: 1));
      yield a; // push a out to the caller
      final next = a + b; // move to the next fibonnaci number
      a = b;
      b = next;
    }
  };
}


extension on Stream {
  Stream<int> Function() streamFilter(bool Function(int) f) {
    return () async* {
      await for (final int n in this) {
        if (f(n)) yield n; // if n is valid push n
      }
    };
  }


  Stream<int> Function() streamAccumulation(int Function(int, int) f, int initial) {
    return () async* {
      await for (final n in this) {
        initial = f(n, initial); // override initial
        yield initial; // push initial
      }
    };
  }
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
  await for (final number in generateNumbers(10)().streamFilter(((a) =>  a % 2 == 0))()) {
    print ('filtered number is $number');
  }
  await for (final number in generateNumbers(10)().streamAccumulation(((a,b) {return a+b;}),0)()) {
    print ('cumulative number is $number.');
  }
}
