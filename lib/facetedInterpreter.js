var interpreter = require('./interpreter.js');

var FacetExecContext = interpreter.ExecutionContext;
var facetedGlobalBase = interpreter.globalBase;

/**
 * Preserving original behaviour of these functions
 */
var BaseExecContext = {
  getValue: interpreter.ExecutionContext.prototype.getValue,
  evalBinOp: interpreter.ExecutionContext.prototype.evalBinOp
};

/**
 * Replacing the existing ExecutionContext constructor to include ProgramCounter in it.
 *
 * @param  number         type
 * @param  bool           strict
 * @param  ProgramCounter programCounter
 */
FacetExecContext.prototype.constructor = function ExecutionContext(type, strict, programCounter) {
  this.type = type;
  this.strict = !!strict;
  this.programCounter = programCounter ? programCounter : new ProgramCounter();
};

/**
 * If called for a facetedValue, a pruned representation based on the current
 * execution context is returned.
 * If v is not a facetedValue then the original behaviour of getValue is invoked.
 *
 * @param  object v
 * @return object
 */
FacetExecContext.prototype.getValue = function(v) {
  if (v instanceof FacetedValue) {
    return v;
  } else {
    return BaseExecContext.getValue.call(this, v);
  }
};

/**
 * Same as getValue, different behavriour for facetedValues and original
 * behaviour for other cases.
 *
 * @param  object v1
 * @param  object v2
 * @param  string op
 *
 * @return object
 */
FacetExecContext.prototype.evalBinOp = function(v1, v2, op) {
  if (v1.label) {
    // TODO: actual operations
    return v1.high + v2.high;
  } else {
    return BaseExecContext.evalBinOp.call(this, v1, v2, op);
  }
};

//TODO: Figure out a better way of organizing labels
const MAX_LABEL= 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz';

/**
 * Returns the label of the root element of the given FacetedValue.
 * For a ProgramCounter, the first label is returned
 * For other types predefined "MAX" label is returned.
 *
 * @param  object v
 * @return string
 */
function head(v) {
  if (v instanceof FacetedValue) {
    return v.label.unsigned();
  }
  if (v instanceof ProgramCounter && v.first()) {
    return v.first().unsigned();
  }
  else return MAX_LABEL;
}

// Sorts alphabetically, but ignores case
function compareLabels(a, b) {
  var al = a.unsigned(),
      bl = b.unsigned();
  if (al === bl) return 0;
  else if (al === MAX_LABEL) return 1;
  else if (bl === MAX_LABEL) return -1;
  else if (al < bl) return -1;
  else return 1;
}

function evaluateEachPair(v1, v2, f, x) {

}

function ProgramCounter() {

}

function Label(value, bar) {

}

/**
 * Representation of a facetedValue as a javascript object
 *
 * @param Label label - Label of principal associated with this value
 * @param object high - authorized view / private value
 * @param object low  - unauthorized view / public value
 */
function FacetedValue(label, high, low) {
  this.label = label;
  this.high = high;
  this.low = low;
};

// FacetedValue.prototype.toString = function() {
//         //return '<' + this.label + '?' + this.authorized + ':' + this.unauthorized + '>';
//         return uneval(this);
// };

facetedGlobalBase.__proto__.FacetedValue = FacetedValue;
