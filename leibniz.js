/*
 * Spencer Kitchen
 * Symbolic calulus
 *
 */


//
// Non-wildcard version of smatch.
//
function smatch1(pattern, target) {
    if (typeof pattern === "number" || typeof pattern == "string") {
        //console.log("got to here 1");
        return pattern === target;          // same number or string
    }
    else
        return pattern instanceof Array &&  // pattern and
               target instanceof Array &&   // target are arrays
               pattern.length === target.length &&    // of the same length
               pattern.every(function(elem, index) {  // and recursively
                   //console.log("got to here 2");
                   return smatch1(elem, target[index]); // contain same elems
               });
}

/*
// TEST 1
console.log("** test one **");
// tests for true pdf:2.1 ----------------------------
console.log(smatch1(42, 42));
console.log(smatch1(["fred", 42], ["fred", 42]));
console.log(smatch1(["fred", [13, "wilma", 94]],
["fred", [13, "wilma", 94]]));
// should get 3 true's in a row
//-----------------------------------------------------

// tests for false pdf:2.1 ----------------------------
console.log(smatch1("fred", "wilma"));
console.log(smatch1(42, "fred"));
console.log(smatch1(42, [42]));
console.log(smatch1([1, 2, 3], [1, 3, 3]))
// should get 4 false in a row
//-----------------------------------------------------
*/

function smatch2(pattern, target) {
    // if it is a numbner
    if (typeof pattern === "number") {
        if (pattern !== target) {
            console.log("got to here 1");
            return null;
        }
    }

    // if it is a string ending in "?"
    else if (typeof pattern === "string" && pattern[(pattern.length - 1)] === "?") {
        console.log("got to here 2");
        return true;
    }

    else if (typeof pattern === "string") {
        if(pattern !== target) {
            console.log("got to here 3");
            return null;
        }
    }

    else
        return pattern instanceof Array &&  // pattern and
               target instanceof Array &&   // target are arrays
               pattern.length === target.length &&    // of the same length
               pattern.every(function(elem, index) {  // and recursively
                   console.log("got to here 4");
                   return smatch2(elem, target[index]); // contain same elems
               });
}

/*
// TEST 2
console.log("** test two **");
// tests for true pdf:2.2 ----------------------------
console.log(smatch2("foo?", 42));
console.log(smatch2("foo?", "fred"));
console.log(smatch2("foo?", [42, "wilma"]));
console.log(smatch2(["x?", [13, "y?"]],["fred", [13, "wilma"]]));
console.log(smatch2(["x?", [13, "y?"]],[[13, 14], [13, "dino"]]));
// should get 5 true's in a row
//-----------------------------------------------------

// tests for false pdf:2.2 ----------------------------
console.log(smatch2(["x?"], 13));
console.log(smatch2(["x?", 5], ["fred", 6]));
console.log(smatch2(["foo", "bar?"], ["foo", "dino", "bambam"]));
// should get 3 false in a row
//-----------------------------------------------------
*/


function smatch(pattern, target, table) {
    table = table || {}
    if (typeof pattern === "number") {
        if (pattern !== target) {
            return null;
        }
    }
    else if (typeof pattern === "string" && pattern[(pattern.length - 1)] === "?") {
        table[pattern.slice(0, (pattern.length -1))] = target;
    }
    else if (typeof pattern === "string") {
        if(pattern !== target) {
            return null;
        }
    }
    else {
        if (!(pattern instanceof Array && target instanceof Array &&
              pattern.length === target.length && pattern.every(function(elem, index) {
                return smatch(elem, target[index], table);
              }))) {
            return null;
        }
    }
    return table;
}

/*
// SMATCH TESTING
console.log("smatch testing");
console.log("new code");
// tests for true pdf:2.2 ----------------------------
console.log(smatch("foo?", 42));
console.log(smatch("foo?", "fred"));
console.log(smatch("foo?", [42, "wilma"]));
//console.log(smatch(["x?", [13, "y?"]],["fred", [13, "wilma"]]));
//console.log(smatch([x?, [13, y?]],[[13, 14], [13, "dino"]]));
// should get 5 true's in a row
//-----------------------------------------------------

var table = {}
console.log(smatch(["x?", [13, "y?"]],[[13, 14], [13, "dino"]], table))
console.log(table.y);
*/

var diffPowerRule = {
    pattern : function(target, table) {
        return smatch(['DERIV', ['^', 'E?', 'N?'], 'V?'], target, table) &&
            typeof table.N === "number";
    },
    transform: function(table) {
        return ['*', ['*', table.N, ['^', table.E, table.N - 1]], 
                ['DERIV', table.E, table.V]];
    },
    label: "diffPowerRule"
};

//
//  d/dt t = 1
//
var diffXRule = {
    pattern : function(target, table) {
        return smatch(['DERIV', 'E?', 'V?'], target, table) &&
            table.E === table.V;
    },
    transform: function(table) {
        return 1;
    },
    label: "diffXRule"
};

//
// (u + v)' = u' + v'
//
var diffSumRule = {
    pattern: function(target, table) {
        
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "diffSumRule"
};

//
// (u - v)' = u' - v'
//
var diffSubtractRule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "diffSubtractRule"
};

//
// d/dt C = 1   (C does not depend on t)
//
var diffConstRule = {
    pattern: function(target, table) {
        return smatch(['DERIV', 'E?', 'V?'], target, table) &&
            table.E === table.V;
        return false;
    },
    transform: function(table) {
        transform: function(table) {
        return 0;
    },
    label: "diffConstRule"
};

//
// (u v)' = uv' + vu'
//
var diffProductRule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "diffProductRule"
};

//
// 3 + 4 = 7   (evaluate constant binary expressions)
//
var foldBinopRule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "foldBinopRule"
};

//
// 3*(2*E) = 6*E  : [*, a, [*, b, e]] => [*, (a*b), e]
//
var foldCoeff1Rule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "foldCoeff1Rule"
};

//
//  x^0 = 1
//
var expt0Rule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "expt0Rule"
};

//
//  x^1 = x
//
var expt1Rule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "expt1Rule"
};

//
//  E * 1 = 1 * E = 0 + E = E + 0 = E
//
var unityRule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "unityRule"
};

//
// E * 0 = 0 * E = 0
//
var times0Rule = {
    pattern: function(target, table) {
        // ...your code here...
        return false;
    },
    transform: function(table) {
        // ...your code here...
    },
    label: "time0Rule"
};

//
// Try to apply "rule" to "expr" recursively -- rule may fire multiple times
// on subexpressions.
// Returns null if rule is *never* applied, else new transformed expression.
// 
function tryRule(rule, expr) {
    var table = {}
    if (!(expr instanceof Array))  // rule patterns match only arrays
        return null;
    else if (rule.pattern(expr, table)) { // rule matches whole expres
        console.log("rule " + rule.label + " fires.");
        return rule.transform(table);     // return transformed expression
    } else { // let's recursively try the rule on each subexpression
        var anyFire = false;
        var newExpr = expr.map(function(e) {
            var t = tryRule(rule, e);
            if (t !== null) {     // note : t = 0 is a valid expression
                anyFire = true;   // at least one rule fired
                return t;         // return transformed subexpression
            } else {
                return e;         // return original subexpression
            }
        });
        return anyFire ? newExpr : null;
    }
}

//
// Try transforming the given expression using all the rules.
// If any rules fire, we return the new transformed expression;
// Otherwise, null is returned.
//
function tryAllRules(expr) {
    var rules = [
        diffPowerRule,
        diffXRule,
        // ... your code here ...
    ];
    // ... your code here ...
    return null;
}

//
// Repeatedly try to reduce expression by applying rules.
// As soon as no more rules fire we are done.
//
function reduceExpr(expr) {
    var e = tryAllRules(expr);
    return (e != null) ? reduceExpr(e) : expr;
}

//if (diffPowerRule.pattern(['DERIV', ['^', 'X', 3], 'X'], table)) {
//     var f = diffPowerRule.transform(table);
//     console.log(f);
// }

//
// Node module exports.
//
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    exports.smatch = smatch;
    exports.diffPowerRule = diffPowerRule;
    exports.tryRule = tryRule;

    exports.diffXRule = diffXRule;
    exports.diffSumRule = diffSumRule;
    exports.diffConstRule = diffConstRule;
    exports.diffProductRule = diffProductRule;
    exports.foldBinopRule = foldBinopRule;
    exports.foldCoeff1Rule = foldCoeff1Rule;
    exports.expt0Rule = expt0Rule;
    exports.expt1Rule = expt1Rule;
    exports.unityRule = unityRule;
    exports.times0Rule = times0Rule;

    exports.tryAllRules = tryAllRules;
    exports.reduceExpr = reduceExpr;
}

