/* -*- Mode: JS; tab-width: 4; indent-tabs-mode: nil; -*-
 * vim: set sw=4 ts=8 et tw=78:
/* ***** BEGIN LICENSE BLOCK *****
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
 * Browser-specific tweaks needed for Narcissus to execute properly
 */

// List of currently running timers
Narcissus.interpreter.stTimers = [];
Narcissus.interpreter.siTimers = [];

// Clears all timers, so that they won't keep running once you have left the page.
Narcissus.interpreter.clearAllTimers = function() {
    var tid;
    for (tid in Narcissus.interpreter.stTimers) clearTimeout(tid);
    for (tid in Narcissus.interpreter.siTimers) clearInterval(tid);
    Narcissus.interpreter.stTimers = [];
    Narcissus.interpreter.siTimers = [];
}


// Prevent setTimeout from breaking out to SpiderMonkey
Narcissus.interpreter.globalBase.setTimeout = function(code, delay) {
    var timeoutCode = (typeof code === "string") ?
            function() { Narcissus.interpreter.evaluate(code); } :
            code;
    var tid = setTimeout(timeoutCode, delay);
    Narcissus.interpreter.stTimers.push(tid);
    return tid;
};

// Prevent setInterval from breaking out to SpiderMonkey
Narcissus.interpreter.globalBase.setInterval = function(code, delay) {
    var timeoutCode = (typeof code === "string") ?
            function() { Narcissus.interpreter.evaluate(code); } :
            code;
    var tid = setInterval(timeoutCode, delay);
    Narcissus.interpreter.siTimers.push(tid);
    return tid;
};

// Hack to avoid problems with the Image constructor in Narcissus.
Narcissus.interpreter.globalBase.Image = function() {};


