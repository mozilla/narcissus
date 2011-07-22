/* -*- Mode: JS; tab-width: 4; indent-tabs-mode: nil; -*-
 * vim: set sw=4 ts=4 et tw=78:
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Narcissus JavaScript engine.
 *
 * The Initial Developer of the Original Code is
 * Brendan Eich <brendan@mozilla.org>.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Tom Austin <taustin@ucsc.edu>
 *   Brendan Eich <brendan@mozilla.org>
 *   Shu-Yu Guo <shu@rfrn.org>
 *   Dave Herman <dherman@mozilla.com>
 *   Dimitris Vardoulakis <dimvar@ccs.neu.edu>
 *   Patrick Walton <pcwalton@mozilla.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/*
 * jsdesugar: eliminate some of the high-level constructs that can be lowered
 * into other core language features.
 *
 * Currently desugared:
 *
 * - multiple catch clauses
 * - catch guards
 * - destructuring
 * - array comprehensions
 * - generator expressions
 */


/*
 * Desugaring of destructuring:
 *
 *     [[ DECL INIT1 ... INITn; ]]          = DECL ((INIT1)) ... ((INITn)), $dummy = (<<INIT1>>, ..., <<INITn>>);
 *     [[ for (DECL? P = E1 in E2) B ]]     = for (DECL? $t = E1 in E2) { [[ P = $t ]]; B }
 *     [[ [P1, ..., Pn] = E ]]              = (let ($t = E) ([[ P1 = $t[0] ]], ..., [[ Pn = $t[n] ]], $t))
 *     [[ { x1: P1, ..., xn: Pn } = E ]]    = (let ($t = E) ([[ P1 = $t.x1 ]], ..., [[ Pn = $t.xn ]], $t))
 *     [[ x = E ]]                          = x = E
 *     [[ function f(P1, ..., Pn) { SS } ]] = function f($t1, ..., $tn) { [[ var P1 = $t1, ..., Pn = $tn; ]] SS }
 *
 *     << P = E >> = [[ P = E ]]
 *     << x >>     = 0
 *
 * Destructuring variable extraction:
 *
 *     (( P = E ))                   = (( P ))
 *     (( x ))                       = x
 *     (( { x1: P1, ..., xn: Pn } )) = (( P1 )), ..., (( Pn ))
 *     (( [ P1, ..., Pn ] ))         = (( P1 )), ..., (( Pn ))
 */

Narcissus.desugaring = (function() {

    var definitions = Narcissus.definitions;
    var parser = Narcissus.parser;

    eval(definitions.consts);

    function id(t, name, readOnly) {
        return new parser.SyntheticNode(t, {
            type: IDENTIFIER,
            value: name,
            name: name,
            readOnly: !!readOnly,
            blockComment: null
        });
    }

    function list(t, elts) {
        // if (elts.length === 0) {
        //     print("EMPTY LIST:");
        //     print((new Error).stack);
        // }
        if (elts.length === 1)
            return elts[0];
        return new parser.SyntheticNode(t, {
            type: COMMA,
            value: ",",
            children: elts,
            parenthesized: true
        });
    }

    // FIXME: currently broken for const, but if jscfa just treats const as var we're ok
    function desugarDeclaration(decl) {
        var vars = [];
        var assignments = [];
        decl.children.forEach(function(init) {
            if (!init.initializer || typeof init.name === "string") {
                vars.push(init);
                return;
            }
            extractBindings(vars, init.name);
            assignments.push(desugarAssignment(varInitializerToAssignment(init)));
        });

        var dummy = generateTemp(decl);
        if (assignments.length > 0) {
            vars.push(new parser.SyntheticNode(decl.tokenizer, {
                type: IDENTIFIER,
                value: dummy.value,
                children: [],
                name: dummy.value,
                // FIXME: handle const
                readOnly: false,
                blockComment: null,
                initializer: list(decl.tokenizer, assignments)
            }));
        }
        [].splice.apply(decl.children, [0, decl.children.length].concat(vars));
    }

    function extractBindings(vars, patt) {
        switch (patt.type) {
          case OBJECT_INIT:
            patt.children.forEach(function(init) {
                extractBindings(vars, init.type === IDENTIFIER ? init : patt.children[1]);
            });
            break;

          case ARRAY_INIT:
            patt.children.forEach(function(patt) {
                extractBindings(vars, patt);
            });
            break;

          case IDENTIFIER:
            vars.push(id(patt.tokenizer, patt.value)); // FIXME: handle const
            break;
        }
    }

    function varInitializerToAssignment(init) {
        return new parser.SyntheticNode(init.tokenizer, {
            type: ASSIGN,
            children: [init.name, init.initializer],
            blockComment: null
        });
    }

    var gensymCounter = 0;

    function gensym(t) {
        var name = "Narcissus desugaring: " + gensymCounter++;
        return new parser.SyntheticNode(t, { type: IDENTIFIER, value: name });
    }

    function generateTemp(node) {
        return gensym(node.tokenizer);
    }

    function withTemp(expr, makeBody) {
        if (expr.type === IDENTIFIER)
            return makeBody(expr);

        var name = generateTemp(expr);
        var body = makeBody(name);
        var binding = new parser.SyntheticNode(expr.tokenizer, {
            type: IDENTIFIER,
            value: name.value,
            name: name.value,
            readOnly: false,
            initializer: expr,
            blockComment: null
        });
        return new parser.SyntheticNode(expr.tokenizer, {
            type: LET_BLOCK,
            varDecls: [binding],
            variables: new parser.SyntheticNode(expr.tokenizer, {
                type: LET,
                value: "(",
                children: [binding],
                destructurings: []
            }),
            expression: body,
            parenthesized: true
        });
    }

    function desugarFunction(node) {
        // expression bodies are desugared to ordinary function bodies
        if (node.body.type !== SCRIPT) {
            var body = node.body;
            // FIXME: this should be abstracted away in jsparse
            node.body = new parser.SyntheticNode(node.tokenizer, {
                type: SCRIPT,
                value: "{",
                children: [new parser.SyntheticNode(node.tokenizer, {
                    type: RETURN,
                    value: body
                })],
                funDecls: [],
                varDecls: [],
                modDefns: new definitions.StringMap(),
                modAssns: new definitions.StringMap(),
                modDecls: new definitions.StringMap(),
                modLoads: new definitions.StringMap(),
                impDecls: [],
                expDecls: [],
                exports: new definitions.StringMap(),
                hasEmptyReturn: false,
                hasReturnWithValue: true,
                isGenerator: false
            });
        }

        var newInits = [];
        node.params.forEach(function(param, i) {
            if (typeof param === "string")
                return;

            var temp = generateTemp(param);
            var init = new parser.SyntheticNode(param.tokenizer, {
                type: IDENTIFIER,
                value: "}",
                children: [],
                name: param,
                readOnly: false,
                blockComment: null,
                initializer: temp
            });
            node.params[i] = temp.value;
            newInits.push(init);
        });
        if (newInits.length > 0) {
            var newDecl = new parser.SyntheticNode(node.tokenizer, {
                type: VAR,
                value: "var",
                children: newInits,
                // FIXME: not correct, but I think we're not using this
                destructurings: [],
                blockComments: []
            });
            node.body.children.unshift(newDecl);
            node.body.varDecls.push(newDecl);
        }
        desugarStatements(node.body);
    }

    function desugarObjectAssignment(lhs, tmp) {
        var elts =
            lhs.children.map(function(init) {
                var propName, patt;
                if (init.type === IDENTIFIER) {
                    propName = patt = id(init.tokenizer, init.value);
                } else {
                    propName = init.children[0];
                    patt = init.children[1];
                }
                var path = new parser.SyntheticNode(lhs.tokenizer, {
                    type: INDEX,
                    value: "[",
                    children: [tmp, new parser.SyntheticNode(lhs.tokenizer, { type: STRING, value: String(propName.value) })]
                });
                return desugarAssignment(new parser.SyntheticNode(lhs.tokenizer, {
                    type: ASSIGN,
                    children: [patt, path],
                    blockComment: null
                }));
            });
        return list(lhs.tokenizer, elts);
    }

    function desugarArrayAssignment(lhs, tmp) {
        var elts =
            lhs.children.map(function(patt, i) {
                if (!patt)
                    return null;
                var path = new parser.SyntheticNode(lhs.tokenizer, {
                    type: INDEX,
                    value: "[",
                    children: [tmp, new parser.SyntheticNode(lhs.tokenizer, { type: NUMBER, value: i })]
                });
                return desugarAssignment(new parser.SyntheticNode(lhs.tokenizer, {
                    type: ASSIGN,
                    children: [patt, path],
                    blockComment: null
                }));
            });
        return list(lhs.tokenizer, elts);
    }

    function desugarAssignment(node) {
        var lhs = node.children[0];
        var rhs = desugarExpression(node.children[1]);
        switch (lhs.type) {
          case OBJECT_INIT:
            return withTemp(rhs, function(tmp) {
                return desugarObjectAssignment(lhs, tmp);
            });

          case ARRAY_INIT:
            return withTemp(rhs, function(tmp) {
                return desugarArrayAssignment(lhs, tmp);
            });
        }
        return node;
    }

    // FIXME: this is not really right; should have a special SEQ node as in SpiderMonkey
    function insertIntoBody(lhs, rhs, body) {
        var assign = new parser.SyntheticNode(body.tokenizer, {
            type: ASSIGN,
            children: [lhs, rhs],
            blockComment: null
        });
        var stmt = new parser.SyntheticNode(body.tokenizer, {
            type: SEMICOLON,
            blockComments: [],
            expression: assign
        });
        if (body.type !== BLOCK) {
            body = new parser.SyntheticNode(body.tokenizer, {
                type: BLOCK,
                value: "{",
                children: [body],
                varDecls: [],
                // FIXME: this is probably wrong if body has label
                labels: new definitions.StringMap()
            });
        }
        body.children.unshift(stmt);
        return body;
    }

    function desugarForInDeclaration(node) {
        if (node.varDecl) {
            // for (var P in E) B
            // for (var P = E1 in E2) B

            // INVARIANT: typeof decl.name === "string" ==> decl === node.iterator
            // INVARIANT: typeof decl.name === "object" ==> decl.name === node.iterator.exp

            var decl = node.varDecl.children[0];

            // FIXME: do this in desugarStatement?
            if (decl.initializer)
                decl.initializer = desugarExpression(decl.initializer);

            if (typeof decl.name !== "string") {
                var patt = decl.name;
                var tmp = generateTemp(patt);
                decl.value = decl.name = tmp.value;
                // FIXME: or generate an ASSIGN node if it's an assignment
                node.iterator = tmp;
                node.body = insertIntoBody(patt, tmp, node.body);
            }
        } else if (node.iterator.type === ASSIGN) {
            // for (P = E1 in E2) B
            var lhs = node.iterator.children[0];

            if (lhs.type !== IDENTIFIER) {
                var tmp = generateTemp(lhs);
                node.iterator.children[0] = tmp;
                node.body = insertIntoBody(lhs, tmp, node.body);
            }
            node.iterator.children[1] = desugarExpression(node.iterator.children[1]);
        } else {
            // for (P in E) B
            if (node.iterator.type !== IDENTIFIER) {
                var tmp = generateTemp(node);
                var patt = node.iterator;
                node.iterator = tmp;
                node.body = insertIntoBody(patt, tmp, node.body);
            }
        }

        // FIXME: do these in desugarStatement?
        node.object = desugarExpression(node.object);
        desugarStatement(node.body);
    }

/*
 * Desugaring of catch clauses:
 *
 *     [[ catch (x1 if e1) B1 ... catch (xn if en) Bn ]]
 *   = catch ($t) {
 *         let $caught = false;
 *         let (x1 = $t) { if (!$caught && e1) { $caught = true; B1 } }
 *         ...
 *         let (xn = $t) { if (!$caught && en) { $caught = true; Bn } }
 *         if (!$caught) throw $t;
 *     }
 *
 *     [[ catch (x1 if e1) B1 ... catch (xn if en) Bn catch (x) B ]]
 *   = catch ($t) {
 *         let $caught = false;
 *         let (x1 = $t) { if (!$caught && e1) { $caught = true; B1 } }
 *         ...
 *         let (xn = $t) { if (!$caught && en) { $caught = true; Bn } }
 *         let (x = $t) { if (!$caught) B }
 *     }
 */

    function desugarCatchClauses(node) {
        var catches = node.catchClauses;
        if (catches.length === 0)
            return;

        if (catches.length === 1 && !catches[0].guard) {
            desugarStatement(catches[0].block);
            return;
        }

        var t = catchNode.tokenizer;

        var tmp = generateTemp(node);
        var caught = generateTemp(node);

        var last = catches.length - 1;
        var stmts = catches.map(function(catchNode, i) {
            if (i === last && !catchNode.guard) {
                // let (x = $t) { if (!$caught) B }
                var init = new parser.SyntheticNode(t, {
                    type: IDENTIFIER,
                    value: catchNode.varName,
                    name: catchNode.varName,
                    readOnly: false,
                    initializer: tmp,
                    blockComment: null
                });
                return new parser.SyntheticNode(t, {
                    type: LET_BLOCK,
                    value: "let",
                    varDecls: [init],
                    variables: new parser.SyntheticNode(t, {
                        type: LET,
                        value: "(",
                        children: [init],
                        destructurings: []
                    }),
                    block: new parser.SyntheticNode(t, {
                        type: BLOCK,
                        value: "{",
                        children: [new parser.SyntheticNode(t, {
                            type: IF,
                            value: "if",
                            condition: new parser.SyntheticNode(t, {
                                type: NOT,
                                value: "!",
                                children: [caught],
                                labels: new definitions.StringMap(),
                                thenPart: catchNode.block,
                                elsePart: null
                            }),
                        })],
                        varDecls: [],
                        labels: new definitions.StringMap()
                    }),
                    blockComments: []
                });
            }

            // let (xi = $t) { if (!$caught && ei) { $caught = true; Bi } }
            var init = new parser.SyntheticNode(t, {
                type: IDENTIFIER,
                value: catchNode.varName,
                name: catchNode.varName,
                readOnly: false,
                initializer: tmp,
                blockComment: null
            });
            return new parser.SyntheticNode(t, {
                type: LET_BLOCK,
                value: "let",
                varDecls: [init],
                variables: new parser.SyntheticNode(t, {
                    type: LET,
                    value: "(",
                    children: [init],
                    destructurings: []
                }),
                block: new parser.SyntheticNode(t, {
                    type: BLOCK,
                    value: "{",
                    children: [new parser.SyntheticNode(t, {
                        type: IF,
                        value: "if",
                        condition: new parser.SyntheticNode(t, {
                            type: AND,
                            value: "&&",
                            children: [new parser.SyntheticNode(t, {
                                type: NOT,
                                value: "!",
                                children: [caught],
                                labels: new definitions.StringMap(),
                                thenPart: new parser.SyntheticNode(t, {
                                    type: BLOCK,
                                    value: "{",
                                    children: [new parser.SyntheticNode(t, {
                                        type: SEMICOLON,
                                        blockComments: [],
                                        expression: new parser.SyntheticNode(t, {
                                            type: ASSIGN,
                                            children: [caught, new parser.SyntheticNode(t, {
                                                type: TRUE,
                                                value: "true"
                                            })],
                                            blockComment: null
                                        })
                                    }), catchNode.block],
                                    varDecls: [],
                                    labels: new definitions.StringMap()
                                }),
                                elsePart: null
                            }), catchNode.guard]
                        })
                    })],
                    varDecls: [],
                    labels: new definitions.StringMap()
                }),
                blockComments: []
            });
        });

        // let $caught = false;
        var initCaught = new parser.SyntheticNode(t, {
            type: LET,
            value: "let",
            children: [new parser.SyntheticNode(t, {
                type: IDENTIFIER,
                value: caught.value,
                name: caught.name,
                readOnly: false,
                initializer: new parser.SyntheticNode(t, {
                    type: FALSE,
                    value: "false"
                }),
                blockComment: null
            })],
            destructurings: [],
            blockComments: []
        });
        stmts.unshift(initCaught);

        if (catches[last].catchGuard) {
            stmts.push(new parser.SyntheticNode(t, {
                type: IF,
                value: "if",
                condition: new parser.SyntheticNode(t, {
                    type: NOT,
                    value: "!",
                    children: [caught],
                    labels: new definitions.StringMap(),
                    thenPart: new parser.SyntheticNode(t, {
                        type: THROW,
                        value: "throw",
                        exception: tmp,
                        blockComments: []
                    }),
                    elsePart: null
                })
            }));
        }

        catches.splice(0, catches.length, new parser.SyntheticNode(t, {
            type: CATCH,
            value: "catch",
            varName: tmp.value,
            block: new parser.SyntheticNode(t, {
                type: BLOCK,
                value: "{",
                children: stmts,
                varDecls: [initCaught],
                labels: new definitions.StringMap()
            })
        }));
    }

    function desugarExpression(node) {
        switch (node.type) {
          case FUNCTION:
            desugarFunction(node);
            return node;

          case ASSIGN:
            return desugarAssignment(node);

          case HOOK:
            node.children[0] = desugarExpression(node.children[0]);
            node.children[1] = desugarExpression(node.children[1]);
            node.children[2] = desugarExpression(node.children[2]);
            return node;

          case OR:
          case AND:
          case BITWISE_OR:
          case BITWISE_XOR:
          case BITWISE_AND:
          case EQ:
          case NE:
          case STRICT_EQ:
          case STRICT_NE:
          case LT:
          case LE:
          case GT:
          case GE:
          case IN:
          case INSTANCEOF:
          case LSH:
          case RSH:
          case URSH:
          case PLUS:
          case MINUS:
          case MUL:
          case DIV:
          case MOD:
          case CALL:
          case NEW_WITH_ARGS:
          case INDEX:
            node.children[0] = desugarExpression(node.children[0]);
            node.children[1] = desugarExpression(node.children[1]);
            return node;

          case DOT:
          case DELETE:
          case VOID:
          case TYPEOF:
          case NOT:
          case BITWISE_NOT:
          case UNARY_PLUS:
          case UNARY_MINUS:
          case NEW:
          case INCREMENT:
          case DECREMENT:
            node.children[0] = desugarExpression(node.children[0]);
            return node;

          case YIELD:
            if (node.value)
                node.value = desugarExpression(node.value);
            return node;

          case COMMA:
          case LIST:
            node.children.forEach(function(child, i) {
                node.children[i] = desugarExpression(child);
            });
            return node;

          case ARRAY_INIT:
            node.children.forEach(function(elt, i) {
                if (elt)
                    node.children[i] = desugarExpression(elt);
            });
            return node;

          case OBJECT_INIT:
            node.children.forEach(function(prop) {
                prop.children[1] = desugarExpression(prop.children[1]);
            });
            return node;

          case NULL:
          case THIS:
          case TRUE:
          case FALSE:
          case NUMBER:
          case STRING:
          case REGEXP:
          case IDENTIFIER:
            return node;

          default:
            throw new Error("unrecognized expression type: " + definitions.tokens[node.type]);
        }
    }

    function desugarStatement(node) {
        switch (node.type) {
          case FUNCTION:
            desugarFunction(node);
            break;

          case VAR:
          case CONST:
          case LET:
            desugarDeclaration(node);
            break;

          case BREAK:
          case CONTINUE:
          case DEBUGGER:
            break;

          case THROW:
            node.exception = desugarExpression(node.exception);
            break;

          case RETURN:
            if (node.value)
                node.value = desugarExpression(node.value);
            break;

          case IF:
            node.condition = desugarExpression(node.condition);
            desugarStatement(node.thenPart);
            if (node.elsePart)
                desugarStatement(node.elsePart);
            return;

          case BLOCK:
            desugarStatements(node);
            break;

          case WHILE:
            node.condition = desugarExpression(node.condition);
            desugarStatement(node.body);
            return;

          case DO:
            desugarStatement(node.body);
            node.condition = desugarExpression(node.condition);
            return;

          case SWITCH:
            node.discriminant = desugarExpression(node.discriminant);
            var cases = node.cases;
            cases.forEach(function(caseNode) {
                if (caseNode.caseLabel)
                    caseNode.caseLabel = desugarExpression(caseNode.caseLabel);
                desugarStatements(caseNode.statements);
            });
            break;

          case FOR:
            if (node.setup) {
                switch (node.setup.type) {
                  case LET:
                  case VAR:
                    desugarDeclaration(node.setup);
                    break;

                  default:
                    node.setup = desugarExpression(node.setup);
                    break;
                }
            }
            if (node.condition)
                node.condition = desugarExpression(node.condition);
            if (node.update)
                node.update = desugarExpression(node.update);
            desugarStatement(node.body);
            break;

          case FOR_IN:
            desugarForInDeclaration(node);
            node.object = desugarExpression(node.object);
            desugarStatement(node.body);
            break;

          case TRY:
            desugarStatement(node.tryBlock);
            desugarCatchClauses(node);
            if (node.finallyBlock)
                desugarStatement(node.finallyBlock);
            break;

          case SEMICOLON:
            if (node.expression)
                node.expression = desugarExpression(node.expression);
            break;

          case LABEL:
            desugarStatement(node.statement);
            break;

          case MODULE:
          case IMPORT:
          case EXPORT:
          default:
            throw new Error("not yet implemented: " + definitions.tokens[node.type]);
        }
    }

    function desugarStatements(node) {
        node.children.forEach(desugarStatement);
    }

    function desugarScript(node) {
        desugarStatements(node);
    }

    function desugar(node) {
        desugarScript(node);
        return node;
    }

    return {
        desugar: desugar
    };

})();
