#include <iostream>

using namespace std;

constexpr unsigned short SEQUENCE_LENGTH = 20;

long nbonacci(const unsigned int series, const unsigned int n) {
    if (series < 2) throw invalid_argument("Series has to be at least 2");

    return 1;
}

void printSequence(const string& title, const unsigned int series) {
    cout << title << endl;
    for (int i = 0; i < SEQUENCE_LENGTH; i++) {
        cout << nbonacci(series, i) << " ";
    }
    cout << endl;
}

void computeNbonacciRatio(const string& title, const unsigned int series) {
    double previousEstimate;
    double currentEstimate = 0.0;
    unsigned int n = 1;
    do {
        previousEstimate = currentEstimate;
        currentEstimate = nbonacci(series, n) / nbonacci(series, n - 1);
        n++;
    } while (abs(currentEstimate - previousEstimate) >= 0.000001);
    cout << title << " ratio approaches " << currentEstimate << " after " << n << " iterations" << endl;
}

int main() {
    printSequence("--- Fibonacci Sequence ---", 2);
    printSequence("--- Tribonacci Sequence ---", 3);
    printSequence("--- Fourbonacci Sequence ---", 4);
    printSequence("--- Fivebonacci Sequence ---", 5);
    cout << endl;
    computeNbonacciRatio("Fibonacci", 2);
    computeNbonacciRatio("Tribonacci", 3);
    computeNbonacciRatio("Fourbonacci", 4);
    computeNbonacciRatio("Fivebonacci", 5);
}
