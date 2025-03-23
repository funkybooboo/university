import 'dart:io';

void main(){

  var name1 = "John";
  String name2 = "John";
  int x = 41;

  print(x);
  dynamic a = 6;
  a = "h";

  const String name3 = "hello";
  final String name4 = "hello";

  // comment

  var myName;
  print(myName);

  myName = "Nate";

  print(myName);

  print("String: $myName");

  var list = [1,2,3];
  print(list);
  print(list[1]);
  list.add(4);
  list.addAll([1,2,3,4]);

  list.insert(3,4);
  list.insertAll(5, [1,2,3,4,5,6]);

  var otherlist = [1,2,3,"a"];

  otherlist.remove(2);

  var dict = {"key": "value"};


  for (var i = 0; i < 10; i++) {
    print(i);
  }

  for (var n in list) {
    print(n);
  }
  int f = 0;
  while (f < 10) {
    print(f);
    f++;
    ++f;
  }

  if (9 > 3) {
    print("yes");
  }
  else if (true) {
    print(dict);
  }
  else {
    print("Poop");
  }


  fact(int x) {
    if (x < 1) return null;
    if (x == 1) return 1;
    return x * fact(x-1);
  }

  print(fact(5));
  print("Give me your input: ");
  String? input = stdin.readLineSync();

  print("input: $input");

  // late means lazy evaluation
  late final n = 5;

  int divide1 = 3 ~/ 4;
  double divide2 = 3 / 4;

  // only assgin if the varible is null
  String? xx;
  xx ??= 'foo';

  // get get the length if its not null
  String? x2;
  x2?.length;
  // if x2 is null take Foo is x2 is not null take x2
  print(x2 ?? "Foo");

  var multiline = """
  
  """;



}

extension Describe on Object? {
  String describe() {
    if (this == null) return "is null";
    else return "is not null";
  }
}
