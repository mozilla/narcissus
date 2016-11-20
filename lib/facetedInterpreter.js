var interpreter = require('./interpreter.js');

// Elements from base interpreter to override for facetedBehaviour
var FacetExecContext = interpreter.ExecutionContext;
var facetedGlobalBase = interpreter.globalBase;

/**
 * Preserving original behaviour of these functions
 */
var BaseExecContext = {
  getValue: interpreter.ExecutionContext.prototype.getValue,
  evalBinOp: interpreter.ExecutionContext.prototype.evalBinOp,
  evalUnaryOp: interpreter.ExecutionContext.prototype.evalUnaryOp
};

/**
 * If called for a facetedValue, a pruned representation based on the current
 * execution context is returned.
 * If v is not a facetedValue then the original behaviour of getValue is invoked.
 *
 * @param  {*} v
 * @return {*}
 */
FacetExecContext.prototype.getValue = function(v, pc) {
  if (v instanceof FacetedValue) {
    if (!pc) {
      var pc = getPC();
    }
    return derefFacetedValue(v, pc);
  } else {
    return BaseExecContext.getValue.call(this, v);
  }
};

/**
 * Prune a FacetedValue to only by stripping out parts that are not relevant
 * to the current programCounter
 *
 * @param  {FacetedValue} v  [description]
 * @param  {ProgramCounter} pc [description]
 *
 * @return {*} v
 */
function derefFacetedValue(v, pc) {
  var high  = v.high,
      low   = v.low,
      label = v.label;
  execCtxt = FacetExecContext.current;
  if (pc.contains(label)) {
    return execCtxt.getValue(high, pc);
  } else if (pc.contains(label.reverse())) {
    return execCtxt.getValue(low, pc);
  } else {
    return buildVal(new ProgramCounter(label),
              execCtxt.getValue(high, pc.join(label)),
              execCtxt.getValue(low, pc.join(label.reverse())));
  }
}

/**
 * TODO: write description
 *
 * @param  {ProgramCounter} pc
 * @param  {FacetedValue} vn
 * @param  {FacetedValue} vo
 *
 * @return {FacetedValue} val
 */
function buildVal(pc, vn, vo) {
  var va = vn ? vn.high : vn,
    vb = vn ? vn.low : vn,
    vc = vo ? vo.high : vo,
    vd = vo ? vo.low : vo,
    rest = pc.rest();
  if (pc.isEmpty()) {
    return vn;
  } else if (head(pc) === head(vn) && head(vn) === head(vo)) {
    let k = vn.label;
    if (!pc.first().bar)
      return new FacetedValue(k, buildVal(rest,va,vc), vd);
    else
      return new FacetedValue(k, vc, buildVal(rest,vb,vd));
  } else if (head(vn) === head(vo) && head(vn) < head(pc)) {
      let k = vn.label;
      return new FacetedValue(k, buildVal(pc,va,vc), buildVal(pc,vb,vd));
  } else if (head(pc) === head(vn) && head(vn) < head(vo)) {
      let k = vn.label;
      if (!pc.first().bar)
          return new FacetedValue(k, buildVal(rest,va,vo), vo);
      else
          return new FacetedValue(k, vo, buildVal(rest,vb,vo));
  } else if (head(pc) === head(vo) && head(vo) < head(vn)) {
      let k = vo.label;
      if (!pc.first().bar)
          return new FacetedValue(k, buildVal(rest,vn,vc), vd);
      else
          return new FacetedValue(k, vc, buildVal(rest,vn,vd));
  } else if (head(pc) < head(vn) && head(pc) < head(vo)) {
      let firstLab = pc.first();
      let k = firstLab.bar ? firstLab.reverse() : firstLab;
      if (!firstLab.bar)
          return new FacetedValue(k, buildVal(rest,vn,vo), vo);
      else
          return new FacetedValue(k, vo, buildVal(rest,vn,vo));
  } else if (head(vn) < head(pc) && head(vn) < head(vo)) {
      let k = vn.label;
      return new FacetedValue(k, buildVal(pc,va,vo), buildVal(pc,vb,vo));
  } else if (head(vo) < head(pc) && head(vo) < head(vn)) {
      let k = vo.label;
      return new FacetedValue(k, buildVal(pc,vn,vc), buildVal(pc,vn,vd));
  } else {
      throw new Error('Unhandled case for buildVal');
  }
}

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
    return evaluateEachPair(v1, v2, function(v1, v2) {
            return eval('v1 ' + op + ' v2');
        }, FacetExecContext.current);
    return v1.high + v2.high;
  } else {
    return BaseExecContext.evalBinOp.call(this, v1, v2, op);
  }
};

/**
 * Another overriding funciton which has a different behaviour for facetedValues
 * and invokes the original behaviour for other types.
 *
 * @param  {*} v
 * @param  {string} op - operator (+|-|!|~)
 *
 * @return {*}
 */
FacetExecContext.prototype.evalUnaryOp = function(v, op) {
  if (v instanceof FacetedValue) {
    return evaluateEach(v, function(v) {
      return eval(op + "v");
    }, FacetExecContext.current);
  } else {
    return BaseExecContext.evalUnaryOp.call(this, v, op);
  }

};

/**
 * Applies the function "f" to both facets of the value v. The function also
 * dereferences facetedValue if applicable.
 *
 * @param  {[type]} v [description]
 * @param  {[type]} f [description]
 * @param  {[type]} x [description]
 * @return {[type]}   [description]
 */
function evaluateEach(v, f, x) {
  let pc = x.programCounter;
  if (!(v instanceof FacetedValue)) {
    return f(v, x);
  }

  if (pc.contains(v.label)) {
    return evaluateEach(v.high, f, x);
  } else if (pc.contains(v.label.reverse())) {
    return evaluateEach(v.low, f, x);
  } else {
    let va, vu;
    try {
      x.pc = pc.join(v.label);
      va = evaluateEach(v.high, f, x);
      x.pc = pc.join(v.label.reverse());
      vu = evaluateEach(v.low, f, x);
      x.pc = pc;
    } catch (e) {
      // Terminate program to avoid leaking data through exceptions
      throw e;
    }
    return new FacetedValue(v.label, va, vu);
  }
}

//TODO: Figure out a better way of organizing labels
const MAX_LABEL = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz';

/**
 * Returns the label of the root element of the given FacetedValue.
 * For a ProgramCounter, the first label is returned
 * For other types predefined "MAX" label is returned.
 *
 * @param  {(FacetedValue|ProgramCounter)} v
 *
 * @return {String} labelValue
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

/**
 * TODO: add description
 *
 * @param  {(FacetedValue|*)} v1
 * @param  {(FacetedValue|*)} v2
 * @param  {Function} f
 * @param  {ExecutionContext} x
 *
 * @return {FacetedValue} facetedValue
 */
function evaluateEachPair(v1, v2, f, x) {
  let pc = x.programCounter;
  if (!(v1 instanceof FacetedValue || v2 instanceof FacetedValue)) {
    return f(v1, v2, x);
  }

  let k = head(v1) < head(v2) ? v1.label : v2.label;

  if (pc.contains(k)) {
    if (head(v1) === head(v2)) {
      return evaluateEachPair(v1.high, v2.high, f, x);
    } else if (v1 && v1.label === k) {
      return evaluateEachPair(v1.high, v2, f, x);
    } else {
      return evaluateEachPair(v1, v2.high, f, x);
    }
  }
  else if (pc.contains(k.reverse())) {
    if (head(v1) === head(v2)) {
      return evaluateEachPair(v1.low, v2.low, f, x);
    } else if (v1 && v1.label === k) {
      return evaluateEachPair(v1.low, v2, f, x);
    } else {
      return evaluateEachPair(v1, v2.low, f, x);
    }
  } else {
    if (head(v1) === head(v2)) {
      let va, vu;
      try {
        x.pc = pc.join(k);
        va = evaluateEachPair(v1.high, v2.high, f, x);
        x.pc = pc.join(k.reverse());
        vu = evaluateEachPair(v1.low, v2.low, f, x);
        x.pc = pc;
      } catch (e) {
          // Terminate program to avoid leaking data through exceptions
          //throw END_SIGNAL;
          throw e;
      }
      return new FacetedValue(k, va, vu);
    } else if (v1 && v1.label === k) {
      let va, vu;
      try {
        x.pc = pc.join(k);
        va = evaluateEachPair(v1.high, v2, f, x);
        x.pc = pc.join(k.reverse());
        vu = evaluateEachPair(v1.low, v2, f, x);
        x.pc = pc;
      } catch (e) {
          // Terminate program to avoid leaking data through exceptions
          //throw END_SIGNAL;
          throw e;
      }
      return new FacetedValue(k, va, vu);
    } else {
      let va, vu;
      try {
        x.pc = pc.join(k);
        va = evaluateEachPair(v1, v2.high, f, x);
        x.pc = pc.join(k.reverse());
        vu = evaluateEachPair(v1, v2.low, f, x);
        x.pc = pc;
      } catch (e) {
          // Terminate program to avoid leaking data through exceptions
          //throw END_SIGNAL;
          throw e;
        }
        return new FacetedValue(k, va, vu);
    }
  }
  throw new Error('Unhandlied case of evaluateEachPair');
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
 * @return {Boolean} result
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
 *
 * @return {Boolean} result
 */
ProgramCounter.prototype.containsStr = function(labelStr) {
    return this.contains(new Label(labelStr));
};

/**
 * Creates a duplicate of the current ProgramCounter and adds 'label' to the
 * new ProgramCounter if it doesn't already exist.
 *
 * @param  {Label} label
 *
 * @return {ProgramCounter} newPC
 */
ProgramCounter.prototype.join = function(label) {
    if (this.contains(label)) return this;
    var newPC = new ProgramCounter();
    // This a way of duplicating the array
    newPC.labelSet = this.labelSet.slice(0);
    newPC.labelSet.push(label);
    newPC.labelSet.sort(compareLabels);
    return newPC;
};

/**
 * Returns first label of the programCounter
 *
 * @return {Label} firstLabel
 */
ProgramCounter.prototype.first = function() {
  if (this.labelSet.length < 1) return null;
  else return this.labelSet[0];
};

/**
 * Returns ProgramCounter object without the first label in the current object
 *
 * @return {ProgramCounter} newPC
 */
ProgramCounter.prototype.rest = function() {
  if (this.labelSet.length < 1) return new ProgramCounter();
  else {
    var newPC = new ProgramCounter();
    newPC.labelSet = this.labelSet.slice(1);
    return newPC;
  }
};

/**
 * @return {Boolean} result
 */
ProgramCounter.prototype.isEmpty = function() {
  return this.labelSet.length < 1;
};

/**
 * @return {string} string
 */
ProgramCounter.prototype.toString = function() {
  return '{' + this.labelSet + '}';
};

/**
 * Label object associated with a prinicipal
 *
 * @param {string} value
 * @param {Boolean} bar
 */
function Label(value, bar) {
  this.value = bar ? value.toUpperCase() : value.toLowerCase();
  this.bar = bar;
};

/**
 * Reverse polarity of label. This helps tracking of implicit flows.
 *
 * @return {Label} reverseLabel
 */
Label.prototype.reverse = function() {
  return new Label(this.value, !this.bar);
};

/**
 * @return {string} string
 */
Label.prototype.unsigned = function() {
  return this.value.toLowerCase();
};

/**
 * @return {string} string
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
 * string representation of FacetedValue
 *
 * @return {string} string
 */
FacetedValue.prototype.toString = function() {
  return '<' + this.label + '?' + this.high + ':' + this.low + '>';
};

/**
 * Sets the programCounter in the current executionContext
 *
 * @param {ProgramCounter} pc
 */
function setPC(pc) {
  FacetExecContext.current.programCounter = pc;
}

/**
 * Returns programCounter of current executionContext
 *
 * @return {ProgramCounter} programCounter
 */
function getPC() {
  if (!FacetExecContext.current.programCounter) {
    // If no programCounter, initialize empty programCounter
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
 * @param  {*} value
 * @param  {string} principal
 *
 * @return {*}
 */
function cloak(principal, highValue, lowValue) {
  if (!principal)
    throw new Error('Must specify a principal.');
  let pc = getPC();
  let lab = new Label(principal);
  if (pc.contains(lab))
    return highValue;
  else if (pc.contains(lab.reverse()))
    return lowValue;
  else
    return new FacetedValue(new Label(principal), highValue, lowValue);
}

// Functions that can be used by programs to incorporate faceted Behaviour.
facetedGlobalBase.__proto__.cloak = cloak;
