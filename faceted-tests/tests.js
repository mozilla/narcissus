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
  this.unaryMinus1 = function() {
    var v1 = 5;
    v2 = -v1;
    assertEquals(-5, v2);
  }

  this.unaryMinus2 = function() {
    var v1 = -5;
    v2 = -v1;
    assertEquals(5, v2);
  }

  this.binaryAddition = function() {
    var v1 = 2;
    var v2 = 3;
    v3 = v1 + v2;
    assertEquals(v3.toString(), "5");
  }

  // Different principles
  this.addTwoFacetedValues1 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("l", 3, 3);
    f3 = f1 + f2;
    assertEquals(f3.toString(), "<h?<l?4:4>:<l?5:5>>");
  };

  // Same principles
  this.addTwoFacetedValues2 = function () {
    var f1 = cloak("h", 1, 2);
    var f2 = cloak("h", 3, 3);
    f3 = f1 + f2;
    assertEquals(f3.toString(), "<h?4:5>");
  };

  this.addFacetedToNonFaceted1 = function() {
    var f1 = cloak("h", 1, 2);
    var v2 = 5;
    f3 = f1 + v2;
    assertEquals(f3.toString(), "<h?6:7>");
  }

}

testCases = new TestCases();
testCases.unaryMinus1();
testCases.unaryMinus2();
testCases.binaryAddition();
testCases.addTwoFacetedValues1();
testCases.addTwoFacetedValues2();
testCases.addFacetedToNonFaceted1();
