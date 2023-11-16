import 'package:flutter/material.dart';

/// Your code should go into the DrawFractal class.

/// The fractal is drawn as an extension of the CustomPainter.
class DrawFractal extends CustomPainter {
  // Level of the fractal, assume 0, no fractal to draw
  var level = 0;

  // Constructor that initializes the level of the fractal
  DrawFractal(level) {
    this.level = level;
  }

  // Recursively draw the fractal
  void fractal(Canvas canvas, int level, Paint paint) {

    if (level == this.level) return; // if at the last level stop

    canvas.save();
    canvas.translate(0, -50); // move the coordinate system
    canvas.scale(0.5, 0.5); // scale down

    drawL(canvas, paint, level); // draw the left line
    drawM(canvas, paint, level); // draw the middle line
    drawR(canvas, paint, level); // draw the right line

    canvas.restore();

  }

  void drawL(Canvas canvas, Paint paint, int level) {
    canvas.save();
    canvas.rotate(0.5236); // rotate clockwise by 30 degrees
    canvas.drawLine(Offset(0, 0), Offset(0,-50), paint); // draw a line
    fractal(canvas, ++level, paint);
    canvas.restore();
  }

  void drawM(Canvas canvas, Paint paint, int level) {
    canvas.save();
    canvas.drawLine(Offset(0, 0), Offset(0,-50), paint); // draw a line
    fractal(canvas, ++level, paint);
    canvas.restore();
  }

  void drawR(Canvas canvas, Paint paint, int level) {
    canvas.save();
    canvas.rotate(-0.5236); // rotate counterclockwise by 30 deggrees
    canvas.drawLine(Offset(0, 0), Offset(0,-50), paint); // draw a line
    fractal(canvas, ++level, paint);
    canvas.restore();
  }

  /// Called when the canvas is (re)painted, perform the initial moving of the
  /// fractal to the center and other actions here such as the initial call to fractal.
  @override
  void paint(Canvas canvas, Size size) {
    canvas.save();

    canvas.drawLine(Offset(0, 0), Offset(0,-50), Paint()
      ..color = Colors.black
      ..style = PaintingStyle.fill);

    fractal(canvas, 1, Paint()
      ..color = Colors.blue
      ..style = PaintingStyle.fill);

    canvas.restore();
  }

  /// Always repaint
  @override
  bool shouldRepaint(CustomPainter oldDelegate) {
    return true;
  }
}

// The code before here is to set up the flutter app and run it. You may not have to
// change this code.

/// Run the application
void main() => runApp(MyApp());

/// The application runs a streambuilder widget
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      // The title of my application
      title: 'My flutter application',
      home: StreamBuilderSetup(),
      debugShowCheckedModeBanner: false,
    );
  }
}

/// Stream to generate numbers from 1 to 10
Stream<int> generateNumbers = (() async* {
  await Future<void>.delayed(const Duration(seconds: 2));
  for (int i = 1; i <= 10; i++) {
    await Future<void>.delayed(const Duration(seconds: 1));
    yield i;
  }
})();


/// Setup a streambuild widget
class StreamBuilderSetup extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return _StreamBuilderSetupState();
  }
}

/// The innards of the streambuilder widget
class _StreamBuilderSetupState extends State<StreamBuilderSetup> {

  /// Boilerplate for initializing the state of the widget
  @override
  initState() {
    super.initState();
  }

  /// Build the widget, it consists of a title and a SizedBox where the
  /// drawing of the fractal (but could be anything takes place)
  /// Most of this is boilerplate code.
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        // Title bar for the widget
        title: const Text('Draw a Fractal in Flutter'),
      ),
      // Box in which to draw the fractal
      body: SizedBox(
        width: double.infinity,
        child: Center(
          child: StreamBuilder<int>(
            stream: generateNumbers,
            initialData: 0,
            builder: (
                BuildContext context,
                AsyncSnapshot<int> snapshot,
                ) {
              if (snapshot.connectionState == ConnectionState.waiting) {
                return Column(
                  crossAxisAlignment: CrossAxisAlignment.center,
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const CircularProgressIndicator(),
                    Visibility(
                      visible: snapshot.hasData,
                      child: Text(
                        snapshot.data.toString(),
                        style:
                        const TextStyle(color: Colors.black, fontSize: 24),
                      ),
                    ),
                  ],
                );
              } else if (snapshot.connectionState == ConnectionState.active ||
                  snapshot.connectionState == ConnectionState.done) {
                if (snapshot.hasError) {
                  return const Text('Error');
                } else if (snapshot.hasData) {
                  return Center(
                    child: CustomPaint(
                      // Set up a canvas of size 500 by 5000
                        size: const Size(500, 500),
                        // Draw the fractal passing the level from the stream
                        painter: DrawFractal(snapshot.data)),
                  );
                } else {
                  return const Text('Empty data');
                }
              } else {
                return Text('State: ${snapshot.connectionState}');
              }
            },
          ),
        ),
      ),
    );
  }
}




