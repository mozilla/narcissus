function assertEquals(expected, actual, message) {
  if (expected === actual) {
    return true;
  } else {
    print("Assertion failed: got " + actual + ", expected " + expected);
    return false;
  }
}

function TestCases() {

  this.testUnaryMinus1 = function() {
    var v1 = 5;
    v2 = -v1;
    return assertEquals(v2, -5);
  };

  this.testUnaryMinus2 = function() {
    var v1 = -5;
    v2 = -v1;
    return assertEquals(v2, 5);
  };

  this.testFacetedUnaryMinus1 = function() {
    var f1 = cloak("h", 1, 2);
    f2 = -f1;
    return assertEquals(f2.toString(), "<h?-1:-2>");
  };

  this.testFacetedUnaryMinus2 = function() {
    var f1 = cloak("h", -1, -2);
    f2 = -f1;
    return assertEquals(f2.toString(), "<h?1:2>");
  };

  this.testBinaryAddition = function() {
    var v1 = 2;
    var v2 = 3;
    v3 = v1 + v2;
    return assertEquals(v3.toString(), "5");
  };

  // Different principles
  this.testAddTwoFacetedValues1 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("l", 3, 3);
    f3 = f1 + f2;
    return assertEquals(f3.toString(), "<h?<l?4:4>:<l?5:5>>");
  };

  // Same principles
  this.testAddTwoFacetedValues2 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("h", 3, 3);
    f3 = f1 + f2;
    return assertEquals(f3.toString(), "<h?4:5>");
  };

  this.testAddFacetedToNonFaceted1 = function() {
    var f1 = cloak("h", 1, 2);
    var v2 = 5;
    f3 = f1 + v2;
    return assertEquals(f3.toString(), "<h?6:7>");
  };

}

testCaseObject = new TestCases();
for (var testCase in testCaseObject) {
  if ("test" === testCase.substring(0,4)) {
    if (testCaseObject[testCase]()) {
      print("Passed");
    }
  }
}
