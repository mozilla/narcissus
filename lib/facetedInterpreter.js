var interpreter = require('./interpreter.js');

// Elements from base interpreter to override for facetedBehaviour
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
 * If called for a facetedValue, a pruned representation based on the current
 * execution context is returned.
 * If v is not a facetedValue then the original behaviour of getValue is invoked.
 *
 * @param  {*} v
 * @return {*}
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
 * @param  {*} v1
 * @param  {*} v2
 * @param  {string} op
 *
 * @return {*}
 */
FacetExecContext.prototype.evalBinOp = function(v1, v2, op) {
  if (v1 instanceof FacetedValue || v2 instanceof FacetedValue) {
    // TODO: actual operations
    return v1.high + v2.high;
  } else {
    return BaseExecContext.evalBinOp.call(this, v1, v2, op);
  }
};

//TODO: Figure out a better way of organizing labels
const MAX_LABEL = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz';

/**
 * Returns the label of the root element of the given FacetedValue.
 * For a ProgramCounter, the first label is returned
 * For other types predefined "MAX" label is returned.
 *
 * @param  {(FacetedValue|ProgramCounter)} v
 * @return {String}
 */
function head(v) {
  if (v instanceof FacetedValue) {
    return v.label.unsigned();
  }
  if (v instanceof ProgramCounter && v.first()) {
    return v.first().unsigned();
  } else return MAX_LABEL;
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
  //TODO: implement
}

/**
 * ProgramCounter keeps track of the principal labels in the current
 * ExecutionContext defining what facets the current execution flow can access.
 *
 * @param {Label} initialLabel
 */
function ProgramCounter(initialLabel) {
  this.labelSet = [];
  if (initialLabel && !(initialLabel instanceof Label))
    throw new Error('Not a label');
  if (initialLabel)
    this.labelSet.push(initialLabel);
}

/**
 * @param  {Label} label
 *
 * @return {Boolean}
 */
ProgramCounter.prototype.contains = function(label) {
  for (var i in this.labelSet) {
    let l = this.labelSet[i];
    if (l.value === label.value) return true;
  }
  return false;
};

/**
 * Only works for lower case strings
 * @param  {string} labelStr
 * @return {Boolean}
 */
ProgramCounter.prototype.containsStr = function(labelStr) {
    return this.contains(new Label(labelStr));
};

/**
 * TODO: add description
 *
 * @param  {Label} label
 * @return ProgramCounter
 */
ProgramCounter.prototype.join = function(label) {
    if (this.contains(label)) return this;
    var newPC = new ProgramCounter();
    newPC.labelSet = this.labelSet.slice(0);
    newPC.labelSet.push(label);
    newPC.labelSet.sort(compareLabels);
    return newPC;
};

/**
 * Returns first label of the programCounter
 *
 * @return {Label}
 */
ProgramCounter.prototype.first = function() {
  if (this.labelSet.length < 1) return null;
  else return this.labelSet[0];
};

/**
 * Returns ProgramCounter object without the first label in the current object
 *
 * @return {ProgramCounter}
 */
ProgramCounter.prototype.rest = function() {
  if (this.labelSet.length < 1) return EMPTY_PC;
  else {
    var newPC = new ProgramCounter();
    newPC.labelSet = this.labelSet.slice(1);
    return newPC;
  }
};

/**
 * @return {Boolean} [description]
 */
ProgramCounter.prototype.isEmpty = function() {
  return this.labelSet.length < 1;
};

/**
 * @return {string}
 */
ProgramCounter.prototype.toString = function() {
  return '{' + this.labelSet + '}';
};

/**
 * Label object associated with a prinicipal
 *
 * @param {string} value
 * @param {Boolean}   bar
 */
function Label(value, bar) {
  this.value = bar ? value.toUpperCase() : value.toLowerCase();
  this.bar = bar;
};

/**
 * Reverse polarity of label. This helps tracking of implicit flows.
 *
 * @return {Label}
 */
Label.prototype.reverse = function() {
  return new Label(this.value, !this.bar);
};

/**
 * @return {string}
 */
Label.prototype.unsigned = function() {
  return this.value.toLowerCase();
};

/**
 * @return {string}
 */
Label.prototype.toString = function() {
  return this.value;
};

/**
 * Representation of a facetedValue
 *
 * @param {Label} label - Label of principal associated with this value
 * @param {*} high - authorized view / private value
 * @param {*} low  - unauthorized view / public value
 */
function FacetedValue(label, high, low) {
  this.label = label;
  this.high = high;
  this.low = low;
}

/**
 * [toString description]
 * @return {[type]} [description]
 */
FacetedValue.prototype.toString = function() {
  //return '<' + this.label + '?' + this.authorized + ':' + this.unauthorized + '>';
  return uneval(this);
};

/**
 * Returns programCounter of current executionContext
 * @return {[type]} [description]
 */
function getPC() {
  if (!FacetExecContext.current.programCounter) {
    FacetExecContext.current.programCounter = new ProgramCounter();
  }
  return FacetExecContext.current.programCounter;
}

/**
 * Returns a facetedValue restricting access to if current programCounter does
 * not contain the 'principal' label or reverse of 'principal' label.
 * If current programCounter contains the 'principal' label then no need to
 * restrict access to 'value'.
 * If current programCounter contains reverse of the 'principal' label then
 * return undefined.
 *
 * @param  {*}      value
 * @param  {string} principal
 *
 * @return {(FacetedValue|{*}|{undefined})}
 */
function cloak(value, principal) {
  if (!principal)
    throw new Error('Must specify a principal.');
  let pc = getPC();
  let lab = new Label(principal);
  if (pc.contains(lab))
    return value;
  else if (pc.contains(lab.reverse()))
    return undefined;
  else
    return new FacetedValue(new Label(principal), value, undefined);
}

// Functions that can be used by programs to incorporate faceted Behaviour.
facetedGlobalBase.__proto__.cloak = cloak;
