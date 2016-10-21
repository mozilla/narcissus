var interpreter = require('./interpreter.js');

var FacetExecContext = interpreter.ExecutionContext;
var facetedGlobalBase = interpreter.globalBase;

var BaseExecContext = {
  getValue: interpreter.ExecutionContext.prototype.getValue,
  evalBinOp: interpreter.ExecutionContext.prototype.evalBinOp
};

FacetExecContext.prototype.getValue = function(v) {
  if (v && v.label) {
    // TODO: Deref faceted Value
    return v;
  } else {
    return BaseExecContext.getValue.call(this, v);
  }
};

FacetExecContext.prototype.evalBinOp = function(v1, v2, op) {
  if (v1.label) {
    // TODO: actual operations
    return v1.high + v2.high;
  } else {
    return BaseExecContext.evalBinOp.call(this, v1, v2, op);
  }
};

function FacetedValue(label, high, low) {
  this.label = label;
  this.high = high;
  this.low = low;
};

FacetedValue.prototype.toString = function() {
        //return '<' + this.label + '?' + this.authorized + ':' + this.unauthorized + '>';
        return uneval(this);
};

facetedGlobalBase.__proto__.FacetedValue = FacetedValue;
