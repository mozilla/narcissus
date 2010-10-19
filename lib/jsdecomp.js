/* vim: set sw=4 ts=4 et tw=78: */
/* ***** BEGIN LICENSE BLOCK *****
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
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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
 * Narcissus - JS implemented in JS.
 *
 * Decompiler and pretty-printer.
 *
 * Notation
 *
 * If an expression e is forwarded to another expression e' on line l, we
 * print:
 *   e /* ~> l|e' */
/*
 * For phi nodes, we print:
 *   φ(l1|op1,...,lN|opN)
 * where l1,...,ln are the original line numbers of op1,...,opN.
 *
 * When a phi node is cyclic, i.e. in the join of loops, we print:
 *   φ(l1|op1,...,μ,...,lN|opN)
 * where μ is anaphoric with respect to the nearest φ.
 */

Narcissus.decompiler = (function() {

    const parser = Narcissus.parser;
    const definitions = Narcissus.definitions;
    const tokens = definitions.tokens;
    const slice = Array.prototype.slice;
    const hasOwnProperty = Object.prototype.hasOwnProperty;

    // Set constants in the local scope.
    eval(definitions.consts);

    const FORWARD_DEPTH = 1;
    const ANNOTATE_INTERNAL = false;

    function indent(n, s) {
        var ss = "", d = true;

        for (var i = 0, j = s.length; i < j; i++) {
            if (d)
                for (var k = 0; k < n; k++)
                    ss += " ";
            ss += s[i];
            d = s[i] == '\n';
        }

        return ss;
    }

    function isBlock(n) {
        return n && (n.type == BLOCK);
    }

    function isNonEmptyBlock(n) {
        return isBlock(n) && n.children.length > 0;
    }

    function pp(o, c, d, inLetHead) {
        var topScript = false;

        if (!o)
            return "";
        if (!(o instanceof Object))
            return o;
        if (!d) {
            topScript = true;
            d = 1;
        }
        if (!c)
            c = [];

        var p = "", n;
        // We have a cycle in a phi node.
        for (var i = 0, j = c.length; i < j; i++) {
            if (o.resolve() === c[i])
                return "μ";
        }

        if (o.forward && d <= FORWARD_DEPTH) {
            var fwd = o.forward;
            o.forward = null;
            p += pp(o, c, d) + " /* ~> ";
            p += fwd.lineno == -1 ? "" : fwd.lineno + "|";
            o.forward = fwd;
            d++;
            n = o.resolve();
            if (n.type == FUNCTION) {
                p += "function */";
                return p;
            }
        } else {
            n = o;
        }

        if (n.parenthesized)
            p += "(";

        switch (n.type) {
          case FUNCTION:
          case GETTER:
          case SETTER:
            if (n.type == FUNCTION)
                p += "function";
            else if (n.type == GETTER)
                p += "get";
            else
                p += "set";

            p += (n.name ? " " + n.name : "") + "(";
            for (var i = 0, j = n.params.length; i < j; i++)
                p += (i > 0 ? ", " : "") + pp(n.params[i], c, d);
            p += ") " + pp(n.body, c, d);
            break;

          case SCRIPT:
          case BLOCK:
            var nc = n.children;
            if (topScript) {
                // No indentation.
                for (var i = 0, j = nc.length; i < j; i++) {
                    if (i > 0)
                        p += "\n";
                    p += pp(nc[i], c, d);
                    var eoc = p[p.length - 1];
                    if (eoc != ";" && eoc != "}")
                        p += ";";
                }

                break;
            }

            p += "{";
            if (n.id !== undefined)
                p += " /* " + n.id + " */";
            p += "\n";
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += "\n";
                p += indent(4, pp(nc[i], c, d));
                var eoc = p[p.length - 1];
                if (eoc != ";" && eoc != "}")
                    p += ";";
            }
            p += "\n}";
            break;

          case LET_BLOCK:
            p += "let (" + pp(n.variables, c, d, true) + ") ";
            if (n.expression)
                p += pp(n.expression, c, d);
            else
                p += pp(n.block, c, d);
            break;

          case IF:
            p += "if (" + pp(n.condition, c, d) + ") ";

            var tp = n.thenPart, ep = n.elsePart;
            var b = isBlock(tp) || isBlock(ep);
            if (!b)
                p += "{\n";
            p += (b ? pp(tp, c, d) : indent(4, pp(tp, c, d))) + (b ? "" : ";\n");

            if (ep) {
                if (!b)
                    p += "} else {\n";
                else
                    p += " else ";

                p += (b ? pp(ep, c, d) : indent(4, pp(ep, c, d))) + (b ? "" : ";\n");
            }
            if (!b)
                p += "}";
            if (n.ssaJoin)
                p += "\n" + pp(n.ssaJoin, c, d);
            break;

          case SWITCH:
            p += "switch (" + pp(n.discriminant, c, d) + ") {\n";
            for (var i = 0, j = n.cases.length; i < j; i++) {
                var ca = n.cases[i];
                if (ca.type == CASE)
                    p += "  case " + pp(ca.caseLabel, c, d) + ":\n";
                else
                    p += "  default:\n";
                if (ca.ssaJoin)
                    p += indent(4, pp(ca.ssaJoin, c, d)) + "\n";
                ps = pp(ca.statements, c, d);
                p += ps.slice(2, ps.length - 2) + "\n";
            }
            p += "}";
            if (n.ssaJoin)
                p += "\n" + pp(n.ssaJoin, c, d);
            break;

          case FOR:
            if (n.ssaJoin)
                p += pp(n.ssaJoin, c, d) + "\n";
            p += "for (" + pp(n.setup, c, d) + "; "
                         + pp(n.condition, c, d) + "; "
                         + pp(n.update, c, d) + ") ";

            var pb = pp(n.body, c, d);
            if (!isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else if (n.body)
                p += pb;
            if (n.ssaBreakJoin)
                p += "\n" + pp(n.ssaBreakJoin, c, d);
            break;

          case WHILE:
            if (n.ssaJoin)
                p += pp(n.ssaJoin, c, d) + "\n";
            p += "while (" + pp(n.condition, c, d) + ") ";

            var pb = pp(n.body, c, d);
            if (!isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else
                p += pb;
            if (n.ssaBreakJoin)
                p += "\n" + pp(n.ssaBreakJoin, c, d);
            break;

          case FOR_IN:
            if (n.ssaJoin)
                p += pp(n.ssaJoin, c, d) + "\n";
            var u = n.varDecl;
            p += n.isEach ? "for each (" : "for (";
            p += (u ? pp(u, c, d) : pp(n.iterator, c, d)) + " in " +
                 pp(n.object, c, d) + ") ";

            var pb = pp(n.body, c, d);
            if (isBlock(n.body))
                p += "{\n" + indent(4, pb) + ";\n}";
            else if (n.body)
                p += pb;
            break;

          case DO:
            if (n.ssaJoin)
                p += pp(n.ssaJoin, c, d) + "\n";
            p += "do " + pp(n.body, c, d);
            p += " while (" + pp(n.condition, c, d) + ");";
            if (n.ssaBreakJoin)
                p += "\n" + pp(n.ssaBreakJoin, c, d);
            break;

          case PHI:
            p += "φ(";
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += ", ";
                // Cycles happen when we forward to a phi node that has ourself in
                // its operands. Pass in n so we know when to break cycles.
                c.push(n);
                p += pp(nc[i], c, d);
                c.pop();
            }
            p += ")";
            break;

          case INTERVENED:
            p += "-";
            break;

          case BREAK:
            p += "break" + (n.label ? " " + n.label : "") + ";";
            break;

          case CONTINUE:
            p += "continue" + (n.label ? " " + n.label : "") + ";";
            break;

          case TRY:
            p += "try ";
            p += pp(n.tryBlock, c, d);
            for (var i = 0, j = n.catchClauses.length; i < j; i++) {
                var t = n.catchClauses[i];
                p += " catch (" + pp(t.varName, c, d) +
                                (t.guard ? " if " + pp(t.guard, c, d) : "") +
                                ") ";
                var pb = pp(t.block, c, d);
                if (t.ssaJoin) {
                    p += "{\n" + indent(4, pp(t.ssaJoin, c, d)) + "\n";
                    p += pb.slice(2);
                } else {
                    p += pb;
                }
            }
            if (n.finallyBlock) {
                p += " finally ";
                var pb = pp(n.finallyBlock, c, d);
                if (n.ssaFinallyJoin) {
                    p += "{\n" + indent(4, pp(n.ssaFinallyJoin, c, d)) + "\n";
                    p += pb.slice(2);
                } else {
                    p += pb;
                }
            }
            if (n.ssaJoin)
                p += "\n" + pp(n.ssaJoin, c, d);
            break;

          case THROW:
            p += "throw " + pp(n.exception, c, d);
            break;

          case RETURN:
            p += "return";
            if (n.value)
              p += " " + pp(n.value, c, d);
            break;

          case YIELD:
            p += "yield";
            if (n.value.type)
              p += " " + pp(n.value, c, d);
            break;

          case GENERATOR:
            p += pp(n.expression, c, d) + " " + pp(n.tail, c, d);
            break;

          case WITH:
            p += "with (" + pp(n.object, c, d) + ") ";
            p += pp(n.body, c, d);
            break;

          case LET:
          case VAR:
          case CONST:
            var nc = n.children;
            if (!inLetHead) {
                p += tokens[n.type] + " ";
            }
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += ", ";
                var u = nc[i];
                if (u.internal && ANNOTATE_INTERNAL)
                    if (d > 1)
                        p += "internal:";
                    else
                        p += "/* internal: */ ";
                p += pp(u.name, c, d);
                if (u.initializer)
                    p += " = " + pp(u.initializer, c, d);
            }
            break;

          case DEBUGGER:
            p += "debugger NYI\n";
            break;

          case SEMICOLON:
            if (n.expression) {
                p += pp(n.expression, c, d) + ";";
            }
            break;

          case LABEL:
            p += n.label + ":\n" + pp(n.statement, c, d);
            break;

          case COMMA:
          case LIST:
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += ", ";
                p += pp(nc[i], c, d);
            }
            break;

          case ASSIGN:
            var nc = n.children;
            var t = n.assignOp;
            p += pp(nc[0], c, d) + " " + (t ? tokens[t] : "") + "="
                                 + " " + pp(nc[1], c, d);
            break;

          case HOOK:
            var nc = n.children;
            p += "(" + pp(nc[0], c, d) + " ? "
                     + pp(nc[1], c, d) + " : "
                     + pp(nc[2], c, d);
            if (n.ssaJoin)
                p += " /* " + pp(n.ssaJoin, c, d) + " */";
            p += ")";
            break;

          case OR:
          case AND:
            var nc = n.children;
            p += "(" + pp(nc[0], c, d) + " " + tokens[n.type] + " "
                     + pp(nc[1], c, d);
            if (n.ssaJoin)
                p += " /* " + pp(n.ssaJoin, c, d) + " */";
            p += ")";
            break;

          case BITWISE_OR:
          case BITWISE_XOR:
          case BITWISE_AND:
          case EQ:
          case NE:
          case STRICT_EQ:
          case STRICT_NE:
          case LT:
          case LE:
          case GE:
          case GT:
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
            var nc = n.children;
            p += "(" + pp(nc[0], c, d) + " " + tokens[n.type] + " "
                     + pp(nc[1], c, d) + ")";
            break;

          case DELETE:
          case VOID:
          case TYPEOF:
            p += tokens[n.type] + " "  + pp(n.children[0], c, d);
            break;

          case NOT:
          case BITWISE_NOT:
            p += tokens[n.type] + pp(n.children[0], c, d);
            break;

          case UNARY_PLUS:
            p += "+" + pp(n.children[0], c, d);
            break;

          case UNARY_MINUS:
            p += "-" + pp(n.children[0], c, d);
            break;

          case INCREMENT:
          case DECREMENT:
            if (n.postfix) {
                p += pp(n.children[0], c, d) + tokens[n.type];
            } else {
                p += tokens[n.type] + pp(n.children[0], c, d);
            }
            break;

          case DOT:
            var nc = n.children;
            p += pp(nc[0], c, d) + "." + pp(nc[1], c, d);
            break;

          case INDEX:
            var nc = n.children;
            p += pp(nc[0], c, d) + "[" + pp(nc[1], c, d) + "]";
            break;

          case CALL:
            var nc = n.children;
            p += pp(nc[0], c, d) + "(" + pp(nc[1], c, d) + ")";
            break;

          case NEW:
          case NEW_WITH_ARGS:
            var nc = n.children;
            p += "new " + pp(nc[0], c, d);
            if (nc[1])
                p += "(" + pp(nc[1], c, d) + ")";
            break;

          case ARRAY_INIT:
            p += "[";
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if(nc[i])
                    p += pp(nc[i], c, d);
                p += ","
            }
            p += "]";
            break;

          case ARRAY_COMP:
            p += "[" + pp (n.expression, c, d) + " ";
            p += pp(n.tail, c, d);
            p += "]";
            break;

          case COMP_TAIL:
            var nc = n.children;
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0)
                    p += " ";
                p += pp(nc[i], c, d);
            }
            if (n.guard)
                p += " if (" + pp(n.guard, c, d) + ")";
            break;

          case OBJECT_INIT:
            var nc = n.children;
            if (nc[0] && nc[0].type == PROPERTY_INIT)
                p += "{\n";
            else
                p += "{";
            for (var i = 0, j = nc.length; i < j; i++) {
                if (i > 0) {
                    if (t.type == PROPERTY_INIT)
                        p += ",\n";
                    else
                        p += ",";
                }

                var t = nc[i];
                if (t.type == PROPERTY_INIT) {
                    var tc = t.children;
                    p += indent(4, pp(tc[0], c, d)) + ": " +
                         indent(4, pp(tc[1], c, d)).substring(4);
                } else {
                    p += pp(t, c, d)
                }
            }
            if (t && t.type == PROPERTY_INIT)
                p += "\n}";
            else
                p += "}";
            break;

          case NULL:
            p += "null";
            break;

          case THIS:
            p += "this";
            break;

          case TRUE:
            p += "true";
            break;

          case FALSE:
            p += "false";
            break;

          case IDENTIFIER:
            if (n.internal && ANNOTATE_INTERNAL) {
                if (d > 1)
                    p += "internal:";
                else
                    p += "/* internal: */ ";
            }
          case NUMBER:
          case REGEXP:
            p += n.value;
            break;

          case STRING:
            p += "\"" + n.value + "\"";
            break;

          case GROUP:
            p += "(" + pp(n.children[0], c, d) + ")";
            break;

          default:
            throw "PANIC: unknown operation " + tokens[n.type] + " " + n.toSource();
        }

        if (fwd)
            p += " */";
        if (n.parenthesized)
            p += ")";

        return p;
    }

    return {
        pp: pp
    };

}());
