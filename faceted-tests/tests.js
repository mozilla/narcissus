function assert(condition, message) {
  if (condition) {
    print("Passed");
  } else {
    if (!message) {
      message = "Assertion failed";
    }
    print(message);
  }
}

function assertEquals(expected, actual, message) {
  assert(expected === actual, message);
}

function TestCases() {
  // unaryMinus1
  this.test1 = function() {
    var v1 = 5;
    v2 = -v1;
    assertEquals(v2, -5);
  };
  
  // unaryMinus2
  this.test2 = function() {
    var v1 = -5;
    v2 = -v1;
    assertEquals(v2, 5);
  };

  // facetedUnaryMinus1
  this.test3 = function() {
    var f1 = cloak("h", 1, 2);
    f2 = -f1;
    assertEquals(f2.toString(), "<h?-1:-2>");
  };

  // facetedUnaryMinus2
  this.test4 = function() {
    var f1 = cloak("h", -1, -2);
    f2 = -f1;
    assertEquals(f2.toString(), "<h?1:2>");
  };

  // binaryAddition
  this.test5 = function() {
    var v1 = 2;
    var v2 = 3;
    v3 = v1 + v2;
    assertEquals(v3.toString(), "5");
  };

  // addTwoFacetedValues1: Different principles
  this.test6 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("l", 3, 3);
    f3 = f1 + f2;
    assertEquals(f3.toString(), "<h?<l?4:4>:<l?5:5>>");
  };

  // addTwoFacetedValues2: Same principles
  this.test7 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("h", 3, 3);
    f3 = f1 + f2;
    assertEquals(f3.toString(), "<h?4:5>");
  };

  // addFacetedToNonFaceted1
  this.test8 = function() {
    var f1 = cloak("h", 1, 2);
    var v2 = 5;
    f3 = f1 + v2;
    assertEquals(f3.toString(), "<h?6:7>");
  };

}

testCases = new TestCases();
var noOfTestCases = 8;
for (var i = 1; i <= noOfTestCases; i++) {
  eval("testCases.test" + i + ".call();");
}
// testCases.unaryMinus1();
// testCases.unaryMinus2();
// testCases.facetedUnaryMinus1();
// testCases.facetedUnaryMinus2();
// testCases.binaryAddition();
// testCases.addTwoFacetedValues1();
// testCases.addTwoFacetedValues2();
// testCases.addFacetedToNonFaceted1();
