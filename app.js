(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$Main$NotFound = {$: 'NotFound'};
var $author$project$Main$EnergyCostCalculator = function (a) {
	return {$: 'EnergyCostCalculator', a: a};
};
var $author$project$Main$EnergyCostCalculatorMsg = function (a) {
	return {$: 'EnergyCostCalculatorMsg', a: a};
};
var $author$project$Main$FrequencyRevolutionsPolesCalculatorMsg = function (a) {
	return {$: 'FrequencyRevolutionsPolesCalculatorMsg', a: a};
};
var $author$project$Main$FrequencyRpmPolesCalculator = function (a) {
	return {$: 'FrequencyRpmPolesCalculator', a: a};
};
var $author$project$Main$PowerTimeEnergyCalculator = function (a) {
	return {$: 'PowerTimeEnergyCalculator', a: a};
};
var $author$project$Main$PowerTimeEnergyCalculatorMsg = function (a) {
	return {$: 'PowerTimeEnergyCalculatorMsg', a: a};
};
var $author$project$Main$VoltageCurrentPowerCalculator = function (a) {
	return {$: 'VoltageCurrentPowerCalculator', a: a};
};
var $author$project$Main$VoltageCurrentPowerCalculatorMsg = function (a) {
	return {$: 'VoltageCurrentPowerCalculatorMsg', a: a};
};
var $author$project$Main$VoltageCurrentResistanceCalculator = function (a) {
	return {$: 'VoltageCurrentResistanceCalculator', a: a};
};
var $author$project$Main$VoltageCurrentResistanceCalculatorMsg = function (a) {
	return {$: 'VoltageCurrentResistanceCalculatorMsg', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Units$Metric$Kilo = {$: 'Kilo'};
var $author$project$Pages$EnergyCostCalculator$NoActiveField = {$: 'NoActiveField'};
var $author$project$Pages$EnergyCostCalculator$TotalCostSolve = {$: 'TotalCostSolve'};
var $author$project$Pages$EnergyCostCalculator$Valid = {$: 'Valid'};
var $author$project$Units$Electricity$WattHours = function (a) {
	return {$: 'WattHours', a: a};
};
var $author$project$Units$Currency$Currency = function (a) {
	return {$: 'Currency', a: a};
};
var $author$project$Pages$EnergyCostCalculator$calculateTotalCost = F2(
	function (_v0, _v1) {
		var wattHoursValue = _v0.a;
		var wattHourCostFloat = _v1.a;
		return $author$project$Units$Currency$Currency(wattHoursValue * wattHourCostFloat);
	});
var $author$project$Units$Metric$ASC = {$: 'ASC'};
var $author$project$Units$Metric$Base = {$: 'Base'};
var $author$project$Units$Electricity$WattHourCost = function (a) {
	return {$: 'WattHourCost', a: a};
};
var $author$project$Units$Metric$prefixToFactor = function (prefix) {
	switch (prefix.$) {
		case 'Base':
			return 1;
		case 'Kilo':
			return 1000;
		case 'Mega':
			return 1000000;
		default:
			return 1000000000;
	}
};
var $author$project$Units$Metric$convertPrefix = F4(
	function (direction, value, oldPrefix, newPrefix) {
		var oldFactor = $author$project$Units$Metric$prefixToFactor(oldPrefix);
		var newFactor = $author$project$Units$Metric$prefixToFactor(newPrefix);
		var multiplier = function () {
			if (direction.$ === 'ASC') {
				return newFactor / oldFactor;
			} else {
				return oldFactor / newFactor;
			}
		}();
		return value * multiplier;
	});
var $author$project$Units$Electricity$floatToEnergyCost = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$WattHourCost(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$ASC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Pages$EnergyCostCalculator$init = function (_v0) {
	var currentTime = _v0.currentTime;
	var energyCost = A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Kilo, 0.12);
	var energy = $author$project$Units$Electricity$WattHours(30000);
	return _Utils_Tuple2(
		{
			activeField: $author$project$Pages$EnergyCostCalculator$NoActiveField,
			energy: energy,
			energyCost: energyCost,
			formStatus: $author$project$Pages$EnergyCostCalculator$Valid,
			solveMethod: $author$project$Pages$EnergyCostCalculator$TotalCostSolve,
			totalCost: A2($author$project$Pages$EnergyCostCalculator$calculateTotalCost, energy, energyCost),
			typedValue: ''
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Units$Electricity$Hertz = function (a) {
	return {$: 'Hertz', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$NoActiveField = {$: 'NoActiveField'};
var $author$project$Pages$FrequencyRpmPolesCalculator$Poles = function (a) {
	return {$: 'Poles', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve = {$: 'RpmSolve'};
var $author$project$Pages$FrequencyRpmPolesCalculator$Valid = {$: 'Valid'};
var $author$project$Pages$FrequencyRpmPolesCalculator$Rpm = function (a) {
	return {$: 'Rpm', a: a};
};
var $elm$core$Basics$truncate = _Basics_truncate;
var $author$project$Pages$FrequencyRpmPolesCalculator$calculateRpm = F2(
	function (_v0, _v1) {
		var hertzValue = _v0.a;
		var polesValue = _v1.a;
		return $author$project$Pages$FrequencyRpmPolesCalculator$Rpm(((hertzValue / ((polesValue / 2) | 0)) * 60) | 0);
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$init = function (_v0) {
	var currentTime = _v0.currentTime;
	var poles = $author$project$Pages$FrequencyRpmPolesCalculator$Poles(2);
	var frequency = $author$project$Units$Electricity$Hertz(60);
	return _Utils_Tuple2(
		{
			activeField: $author$project$Pages$FrequencyRpmPolesCalculator$NoActiveField,
			formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Valid,
			frequency: frequency,
			poles: poles,
			rpm: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateRpm, frequency, poles),
			solveMethod: $author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve,
			typedValue: ''
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve = {$: 'EnergySolve'};
var $author$project$Pages$PowerTimeEnergyCalculator$NoActiveField = {$: 'NoActiveField'};
var $author$project$Units$Time$Seconds = function (a) {
	return {$: 'Seconds', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$Valid = {$: 'Valid'};
var $author$project$Units$Electricity$Watts = function (a) {
	return {$: 'Watts', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$init = function (_v0) {
	var currentTime = _v0.currentTime;
	var power = $author$project$Units$Electricity$Watts(30000);
	var energy = $author$project$Units$Electricity$WattHours(30000);
	return _Utils_Tuple2(
		{
			activeField: $author$project$Pages$PowerTimeEnergyCalculator$NoActiveField,
			duration: $author$project$Units$Time$Seconds(60 * 60),
			energy: energy,
			formStatus: $author$project$Pages$PowerTimeEnergyCalculator$Valid,
			power: power,
			solveMethod: $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve,
			typedValue: ''
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Units$Electricity$Amps = function (a) {
	return {$: 'Amps', a: a};
};
var $author$project$Pages$VoltageCurrentPowerCalculator$NoActiveField = {$: 'NoActiveField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve = {$: 'PowerSolve'};
var $author$project$Pages$VoltageCurrentPowerCalculator$Valid = {$: 'Valid'};
var $author$project$Units$Electricity$Volts = function (a) {
	return {$: 'Volts', a: a};
};
var $author$project$Pages$VoltageCurrentPowerCalculator$calculatePower = F2(
	function (_v0, _v1) {
		var voltsValue = _v0.a;
		var ampsValue = _v1.a;
		return $author$project$Units$Electricity$Watts(voltsValue * ampsValue);
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$init = function (_v0) {
	var currentTime = _v0.currentTime;
	var voltage = $author$project$Units$Electricity$Volts(30000);
	var current = $author$project$Units$Electricity$Amps(1000);
	return _Utils_Tuple2(
		{
			activeField: $author$project$Pages$VoltageCurrentPowerCalculator$NoActiveField,
			current: current,
			formStatus: $author$project$Pages$VoltageCurrentPowerCalculator$Valid,
			power: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculatePower, voltage, current),
			solveMethod: $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve,
			typedValue: '',
			voltage: voltage
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$NoActiveField = {$: 'NoActiveField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve = {$: 'ResistanceSolve'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$Valid = {$: 'Valid'};
var $author$project$Units$Electricity$Ohms = function (a) {
	return {$: 'Ohms', a: a};
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$calculateResistance = F2(
	function (_v0, _v1) {
		var voltsValue = _v0.a;
		var ampsValue = _v1.a;
		return $author$project$Units$Electricity$Ohms(voltsValue / ampsValue);
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$init = function (_v0) {
	var currentTime = _v0.currentTime;
	var voltage = $author$project$Units$Electricity$Volts(30000);
	var current = $author$project$Units$Electricity$Amps(1000);
	return _Utils_Tuple2(
		{
			activeField: $author$project$Pages$VoltageCurrentResistanceCalculator$NoActiveField,
			current: current,
			formStatus: $author$project$Pages$VoltageCurrentResistanceCalculator$Valid,
			resistance: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateResistance, voltage, current),
			solveMethod: $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve,
			typedValue: '',
			voltage: voltage
		},
		$elm$core$Platform$Cmd$none);
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$initCurrentPage = function (_v0) {
	var model = _v0.a;
	var existingCmds = _v0.b;
	var _v1 = function () {
		var _v2 = model.route;
		switch (_v2.$) {
			case 'NotFound':
				return _Utils_Tuple2($author$project$Main$NotFound, $elm$core$Platform$Cmd$none);
			case 'Home':
				var _v3 = $author$project$Pages$PowerTimeEnergyCalculator$init(model.config);
				var pageModel = _v3.a;
				var pageCmds = _v3.b;
				return _Utils_Tuple2(
					$author$project$Main$PowerTimeEnergyCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$PowerTimeEnergyCalculatorMsg, pageCmds));
			case 'PowerTimeEnergyCalculator':
				var _v4 = $author$project$Pages$PowerTimeEnergyCalculator$init(model.config);
				var pageModel = _v4.a;
				var pageCmds = _v4.b;
				return _Utils_Tuple2(
					$author$project$Main$PowerTimeEnergyCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$PowerTimeEnergyCalculatorMsg, pageCmds));
			case 'VoltageCurrentPowerCalculator':
				var _v5 = $author$project$Pages$VoltageCurrentPowerCalculator$init(model.config);
				var pageModel = _v5.a;
				var pageCmds = _v5.b;
				return _Utils_Tuple2(
					$author$project$Main$VoltageCurrentPowerCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$VoltageCurrentPowerCalculatorMsg, pageCmds));
			case 'VoltageCurrentResistanceCalculator':
				var _v6 = $author$project$Pages$VoltageCurrentResistanceCalculator$init(model.config);
				var pageModel = _v6.a;
				var pageCmds = _v6.b;
				return _Utils_Tuple2(
					$author$project$Main$VoltageCurrentResistanceCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$VoltageCurrentResistanceCalculatorMsg, pageCmds));
			case 'EnergyCostCalculator':
				var _v7 = $author$project$Pages$EnergyCostCalculator$init(model.config);
				var pageModel = _v7.a;
				var pageCmds = _v7.b;
				return _Utils_Tuple2(
					$author$project$Main$EnergyCostCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$EnergyCostCalculatorMsg, pageCmds));
			default:
				var _v8 = $author$project$Pages$FrequencyRpmPolesCalculator$init(model.config);
				var pageModel = _v8.a;
				var pageCmds = _v8.b;
				return _Utils_Tuple2(
					$author$project$Main$FrequencyRpmPolesCalculator(pageModel),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$FrequencyRevolutionsPolesCalculatorMsg, pageCmds));
		}
	}();
	var currentPage = _v1.a;
	var mappedPageCmds = _v1.b;
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{page: currentPage}),
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[existingCmds, mappedPageCmds])));
};
var $author$project$Routes$NotFound = {$: 'NotFound'};
var $author$project$Routes$EnergyCostCalculator = {$: 'EnergyCostCalculator'};
var $author$project$Routes$FrequencyRpmPolesCalculator = {$: 'FrequencyRpmPolesCalculator'};
var $author$project$Routes$Home = {$: 'Home'};
var $author$project$Routes$PowerTimeEnergyCalculator = {$: 'PowerTimeEnergyCalculator'};
var $author$project$Routes$VoltageCurrentPowerCalculator = {$: 'VoltageCurrentPowerCalculator'};
var $author$project$Routes$VoltageCurrentResistanceCalculator = {$: 'VoltageCurrentResistanceCalculator'};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Routes$matchRoute = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Routes$Home, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Routes$PowerTimeEnergyCalculator,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('electricity'),
				$elm$url$Url$Parser$s('power-time-energy'))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Routes$VoltageCurrentPowerCalculator,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('electricity'),
				$elm$url$Url$Parser$s('voltage-current-power'))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Routes$VoltageCurrentResistanceCalculator,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('electricity'),
				$elm$url$Url$Parser$s('voltage-current-resistance'))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Routes$EnergyCostCalculator,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('electricity'),
				$elm$url$Url$Parser$s('energy-cost'))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Routes$FrequencyRpmPolesCalculator,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('electricity'),
				$elm$url$Url$Parser$s('frequency-revolutions-poles')))
		]));
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Routes$parseUrl = function (url) {
	var _v0 = A2($elm$url$Url$Parser$parse, $author$project$Routes$matchRoute, url);
	if (_v0.$ === 'Just') {
		var route = _v0.a;
		return route;
	} else {
		return $author$project$Routes$NotFound;
	}
};
var $author$project$Main$init = F3(
	function (flags, url, navKey) {
		var model = {
			config: flags,
			navKey: navKey,
			page: $author$project$Main$NotFound,
			route: $author$project$Routes$parseUrl(url)
		};
		return $author$project$Main$initCurrentPage(
			_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Pages$EnergyCostCalculator$EnergyCostSolve = {$: 'EnergyCostSolve'};
var $author$project$Pages$EnergyCostCalculator$EnergySolve = {$: 'EnergySolve'};
var $author$project$Units$Metric$Giga = {$: 'Giga'};
var $author$project$Pages$EnergyCostCalculator$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $author$project$Units$Metric$Mega = {$: 'Mega'};
var $author$project$Pages$EnergyCostCalculator$calculateEnergy = F2(
	function (_v0, _v1) {
		var totalCost = _v0.a;
		var energyCost = _v1.a;
		return $author$project$Units$Electricity$WattHours(totalCost / energyCost);
	});
var $author$project$Pages$EnergyCostCalculator$calculateWattHourCost = F2(
	function (_v0, _v1) {
		var totalCostValue = _v0.a;
		var wattHoursValue = _v1.a;
		return $author$project$Units$Electricity$WattHourCost(totalCostValue / wattHoursValue);
	});
var $author$project$Units$Metric$DESC = {$: 'DESC'};
var $author$project$Units$Electricity$floatToEnergy = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$WattHours(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Units$Number$numberStringToFloat = function (numberString) {
	return $elm$core$String$toFloat(
		A3($elm$core$String$replace, ',', '', numberString));
};
var $author$project$Pages$EnergyCostCalculator$updateTotalCost = F2(
	function (model, totalCost) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'EnergySolve':
				return _Utils_update(
					model,
					{
						energy: A2($author$project$Pages$EnergyCostCalculator$calculateEnergy, totalCost, model.energyCost),
						totalCost: totalCost
					});
			case 'EnergyCostSolve':
				return _Utils_update(
					model,
					{
						energyCost: A2($author$project$Pages$EnergyCostCalculator$calculateWattHourCost, totalCost, model.energy),
						totalCost: totalCost
					});
			default:
				return model;
		}
	});
var $author$project$Pages$EnergyCostCalculator$updateWattHourCost = F2(
	function (model, energyCost) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'EnergySolve':
				return _Utils_update(
					model,
					{
						energy: A2($author$project$Pages$EnergyCostCalculator$calculateEnergy, model.totalCost, energyCost),
						energyCost: energyCost
					});
			case 'TotalCostSolve':
				return _Utils_update(
					model,
					{
						energyCost: energyCost,
						totalCost: A2($author$project$Pages$EnergyCostCalculator$calculateTotalCost, model.energy, energyCost)
					});
			default:
				return model;
		}
	});
var $author$project$Pages$EnergyCostCalculator$updateWattHours = F2(
	function (model, energy) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'EnergyCostSolve':
				return _Utils_update(
					model,
					{
						energy: energy,
						energyCost: A2($author$project$Pages$EnergyCostCalculator$calculateWattHourCost, model.totalCost, energy)
					});
			case 'TotalCostSolve':
				return _Utils_update(
					model,
					{
						energy: energy,
						totalCost: A2($author$project$Pages$EnergyCostCalculator$calculateTotalCost, energy, model.energyCost)
					});
			default:
				return model;
		}
	});
var $author$project$Pages$EnergyCostCalculator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSolveMethod':
				var solveMethod = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{solveMethod: solveMethod}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateField':
				var field = msg.a;
				var typedValue = msg.b;
				var _v1 = $author$project$Units$Number$numberStringToFloat(typedValue);
				if (_v1.$ === 'Just') {
					var floatValue = _v1.a;
					var newModel = function () {
						switch (field.$) {
							case 'WattHoursField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHours,
									model,
									$author$project$Units$Electricity$WattHours(floatValue));
							case 'KilowattHoursField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHours,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegawattHoursField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHours,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Mega, floatValue));
							case 'GigawattHoursField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHours,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Giga, floatValue));
							case 'WattHourCostField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHourCost,
									model,
									$author$project$Units$Electricity$WattHourCost(floatValue));
							case 'KilowattHourCostField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHourCost,
									model,
									A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegawattHourCostField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHourCost,
									model,
									A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Mega, floatValue));
							case 'GigawattHourCostField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateWattHourCost,
									model,
									A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Giga, floatValue));
							case 'TotalCostField':
								return A2(
									$author$project$Pages$EnergyCostCalculator$updateTotalCost,
									model,
									$author$project$Units$Currency$Currency(floatValue));
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: field, formStatus: $author$project$Pages$EnergyCostCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: field,
								formStatus: $author$project$Pages$EnergyCostCalculator$Invalid('must be a number'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var example = msg.a;
				var newModel = function () {
					switch (example.$) {
						case 'EnergyExample':
							var totalCost = example.a;
							var energyCost = example.b;
							return _Utils_update(
								model,
								{
									energy: A2($author$project$Pages$EnergyCostCalculator$calculateEnergy, totalCost, energyCost),
									energyCost: energyCost,
									solveMethod: $author$project$Pages$EnergyCostCalculator$EnergySolve,
									totalCost: totalCost
								});
						case 'WattHourCostExample':
							var totalCost = example.a;
							var energy = example.b;
							return _Utils_update(
								model,
								{
									energy: energy,
									energyCost: A2($author$project$Pages$EnergyCostCalculator$calculateWattHourCost, totalCost, energy),
									solveMethod: $author$project$Pages$EnergyCostCalculator$EnergyCostSolve,
									totalCost: totalCost
								});
						default:
							var energy = example.a;
							var energyCost = example.b;
							return _Utils_update(
								model,
								{
									energy: energy,
									energyCost: energyCost,
									solveMethod: $author$project$Pages$EnergyCostCalculator$TotalCostSolve,
									totalCost: A2($author$project$Pages$EnergyCostCalculator$calculateTotalCost, energy, energyCost)
								});
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{activeField: $author$project$Pages$EnergyCostCalculator$NoActiveField, formStatus: $author$project$Pages$EnergyCostCalculator$Valid}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve = {$: 'FrequencySolve'};
var $author$project$Pages$FrequencyRpmPolesCalculator$HertzField = {$: 'HertzField'};
var $author$project$Pages$FrequencyRpmPolesCalculator$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$PolesField = {$: 'PolesField'};
var $author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve = {$: 'PolesSolve'};
var $author$project$Pages$FrequencyRpmPolesCalculator$RpmField = {$: 'RpmField'};
var $author$project$Pages$FrequencyRpmPolesCalculator$calculateFrequency = F2(
	function (_v0, _v1) {
		var polesValue = _v0.a;
		var rpmValue = _v1.a;
		return $author$project$Units$Electricity$Hertz((rpmValue / ((polesValue / 2) | 0)) / 60);
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$calculatePoles = F2(
	function (_v0, _v1) {
		var hertzValue = _v0.a;
		var rpmValue = _v1.a;
		return $author$project$Pages$FrequencyRpmPolesCalculator$Poles((((rpmValue / hertzValue) / 60) * 2) | 0);
	});
var $author$project$Units$Number$isEven = function (value) {
	return !(value % 2);
};
var $author$project$Units$Number$numberStringToInt = function (numberString) {
	return $elm$core$String$toInt(
		A3($elm$core$String$replace, ',', '', numberString));
};
var $author$project$Pages$FrequencyRpmPolesCalculator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSolveMethod':
				var solveMethod = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{solveMethod: solveMethod}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateHertzField':
				var typedValue = msg.a;
				var _v1 = $author$project$Units$Number$numberStringToFloat(typedValue);
				if (_v1.$ === 'Just') {
					var floatValue = _v1.a;
					var frequency = $author$project$Units$Electricity$Hertz(floatValue);
					var newModel = function () {
						var _v2 = model.solveMethod;
						switch (_v2.$) {
							case 'PolesSolve':
								return _Utils_update(
									model,
									{
										frequency: frequency,
										poles: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculatePoles, frequency, model.rpm)
									});
							case 'RpmSolve':
								return _Utils_update(
									model,
									{
										frequency: frequency,
										rpm: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateRpm, frequency, model.poles)
									});
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: $author$project$Pages$FrequencyRpmPolesCalculator$HertzField, formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: $author$project$Pages$FrequencyRpmPolesCalculator$HertzField,
								formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Invalid('must be a number'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UpdatePolesField':
				var typedValue = msg.a;
				var _v3 = $author$project$Units$Number$numberStringToInt(typedValue);
				if (_v3.$ === 'Just') {
					var intValue = _v3.a;
					if ($author$project$Units$Number$isEven(intValue)) {
						var poles = $author$project$Pages$FrequencyRpmPolesCalculator$Poles(intValue);
						var newModel = function () {
							var _v4 = model.solveMethod;
							switch (_v4.$) {
								case 'FrequencySolve':
									return _Utils_update(
										model,
										{
											frequency: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateFrequency, poles, model.rpm),
											poles: poles
										});
								case 'RpmSolve':
									return _Utils_update(
										model,
										{
											poles: poles,
											rpm: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateRpm, model.frequency, poles)
										});
								default:
									return model;
							}
						}();
						return _Utils_Tuple2(
							_Utils_update(
								newModel,
								{activeField: $author$project$Pages$FrequencyRpmPolesCalculator$PolesField, formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Valid, typedValue: typedValue}),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									activeField: $author$project$Pages$FrequencyRpmPolesCalculator$PolesField,
									formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Invalid('must be an even integer'),
									typedValue: typedValue
								}),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: $author$project$Pages$FrequencyRpmPolesCalculator$PolesField,
								formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Invalid('must be an even integer'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UpdateRpmField':
				var typedValue = msg.a;
				var _v5 = $author$project$Units$Number$numberStringToInt(typedValue);
				if (_v5.$ === 'Just') {
					var intValue = _v5.a;
					var rpm = $author$project$Pages$FrequencyRpmPolesCalculator$Rpm(intValue);
					var newModel = function () {
						var _v6 = model.solveMethod;
						switch (_v6.$) {
							case 'FrequencySolve':
								return _Utils_update(
									model,
									{
										frequency: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateFrequency, model.poles, rpm),
										rpm: rpm
									});
							case 'PolesSolve':
								return _Utils_update(
									model,
									{
										poles: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculatePoles, model.frequency, rpm),
										rpm: rpm
									});
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: $author$project$Pages$FrequencyRpmPolesCalculator$RpmField, formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: $author$project$Pages$FrequencyRpmPolesCalculator$RpmField,
								formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Invalid('must be an integer'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var example = msg.a;
				var newModel = function () {
					switch (example.$) {
						case 'FrequencyExample':
							var rpm = example.a;
							var poles = example.b;
							return _Utils_update(
								model,
								{
									frequency: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateFrequency, poles, rpm),
									poles: poles,
									rpm: rpm,
									solveMethod: $author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve
								});
						case 'RpmExample':
							var frequency = example.a;
							var poles = example.b;
							return _Utils_update(
								model,
								{
									frequency: frequency,
									poles: poles,
									rpm: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculateRpm, frequency, poles),
									solveMethod: $author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve
								});
						default:
							var frequency = example.a;
							var rpm = example.b;
							return _Utils_update(
								model,
								{
									frequency: frequency,
									poles: A2($author$project$Pages$FrequencyRpmPolesCalculator$calculatePoles, frequency, rpm),
									rpm: rpm,
									solveMethod: $author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve
								});
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{activeField: $author$project$Pages$FrequencyRpmPolesCalculator$NoActiveField, formStatus: $author$project$Pages$FrequencyRpmPolesCalculator$Valid}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Units$Time$Days = function (a) {
	return {$: 'Days', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$DurationSolve = {$: 'DurationSolve'};
var $author$project$Units$Time$Hours = function (a) {
	return {$: 'Hours', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $author$project$Units$Time$Minutes = function (a) {
	return {$: 'Minutes', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve = {$: 'PowerSolve'};
var $author$project$Pages$PowerTimeEnergyCalculator$calculateDuration = F2(
	function (_v0, _v1) {
		var wattsValue = _v0.a;
		var wattHoursValue = _v1.a;
		return $author$project$Units$Time$Seconds((wattHoursValue / wattsValue) * 3600);
	});
var $author$project$Units$Time$hoursToFloat = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Units$Time$secondsToHours = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Hours((value / 60) / 60);
};
var $author$project$Pages$PowerTimeEnergyCalculator$calculateEnergy = F2(
	function (_v0, duration) {
		var wattsValue = _v0.a;
		return $author$project$Units$Electricity$WattHours(
			wattsValue * $author$project$Units$Time$hoursToFloat(
				$author$project$Units$Time$secondsToHours(duration)));
	});
var $author$project$Pages$PowerTimeEnergyCalculator$calculatePower = F2(
	function (_v0, duration) {
		var wattHoursValue = _v0.a;
		return $author$project$Units$Electricity$Watts(
			wattHoursValue / $author$project$Units$Time$hoursToFloat(
				$author$project$Units$Time$secondsToHours(duration)));
	});
var $author$project$Units$Time$daysToSeconds = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Seconds(((value * 24) * 60) * 60);
};
var $author$project$Units$Electricity$floatToPower = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$Watts(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $author$project$Units$Time$hoursToSeconds = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Seconds((value * 60) * 60);
};
var $author$project$Units$Time$minutesToSeconds = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Seconds(value * 60);
};
var $author$project$Pages$PowerTimeEnergyCalculator$updateDuration = F2(
	function (model, duration) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'PowerSolve':
				return _Utils_update(
					model,
					{
						duration: duration,
						power: A2($author$project$Pages$PowerTimeEnergyCalculator$calculatePower, model.energy, duration)
					});
			case 'EnergySolve':
				return _Utils_update(
					model,
					{
						duration: duration,
						energy: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateEnergy, model.power, duration)
					});
			default:
				return model;
		}
	});
var $author$project$Pages$PowerTimeEnergyCalculator$updateEnergy = F2(
	function (model, energy) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'PowerSolve':
				return _Utils_update(
					model,
					{
						energy: energy,
						power: A2($author$project$Pages$PowerTimeEnergyCalculator$calculatePower, energy, model.duration)
					});
			case 'DurationSolve':
				return _Utils_update(
					model,
					{
						duration: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateDuration, model.power, energy),
						energy: energy
					});
			default:
				return model;
		}
	});
var $author$project$Pages$PowerTimeEnergyCalculator$updatePower = F2(
	function (model, power) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'EnergySolve':
				return _Utils_update(
					model,
					{
						energy: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateEnergy, power, model.duration),
						power: power
					});
			case 'DurationSolve':
				return _Utils_update(
					model,
					{
						duration: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateDuration, power, model.energy),
						power: power
					});
			default:
				return model;
		}
	});
var $author$project$Pages$PowerTimeEnergyCalculator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSolveMethod':
				var solveMethod = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{solveMethod: solveMethod}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateField':
				var field = msg.a;
				var typedValue = msg.b;
				var _v1 = $author$project$Units$Number$numberStringToFloat(typedValue);
				if (_v1.$ === 'Just') {
					var floatValue = _v1.a;
					var newModel = function () {
						switch (field.$) {
							case 'WattsField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updatePower,
									model,
									$author$project$Units$Electricity$Watts(floatValue));
							case 'KilowattsField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegawattsField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Mega, floatValue));
							case 'GigawattsField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Giga, floatValue));
							case 'WattHoursField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateEnergy,
									model,
									$author$project$Units$Electricity$WattHours(floatValue));
							case 'KilowattHoursField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateEnergy,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegawattHoursField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateEnergy,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Mega, floatValue));
							case 'GigawattHoursField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateEnergy,
									model,
									A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Giga, floatValue));
							case 'SecondsField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateDuration,
									model,
									$author$project$Units$Time$Seconds(floatValue));
							case 'MinutesField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateDuration,
									model,
									$author$project$Units$Time$minutesToSeconds(
										$author$project$Units$Time$Minutes(floatValue)));
							case 'HoursField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateDuration,
									model,
									$author$project$Units$Time$hoursToSeconds(
										$author$project$Units$Time$Hours(floatValue)));
							case 'DaysField':
								return A2(
									$author$project$Pages$PowerTimeEnergyCalculator$updateDuration,
									model,
									$author$project$Units$Time$daysToSeconds(
										$author$project$Units$Time$Days(floatValue)));
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: field, formStatus: $author$project$Pages$PowerTimeEnergyCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: field,
								formStatus: $author$project$Pages$PowerTimeEnergyCalculator$Invalid('must be a number'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var example = msg.a;
				var newModel = function () {
					switch (example.$) {
						case 'EnergyExample':
							var power = example.a;
							var duration = example.b;
							return _Utils_update(
								model,
								{
									duration: duration,
									energy: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateEnergy, power, duration),
									power: power,
									solveMethod: $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve
								});
						case 'PowerExample':
							var energy = example.a;
							var duration = example.b;
							return _Utils_update(
								model,
								{
									duration: duration,
									energy: energy,
									power: A2($author$project$Pages$PowerTimeEnergyCalculator$calculatePower, energy, duration),
									solveMethod: $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve
								});
						default:
							var power = example.a;
							var energy = example.b;
							return _Utils_update(
								model,
								{
									duration: A2($author$project$Pages$PowerTimeEnergyCalculator$calculateDuration, power, energy),
									energy: energy,
									power: power,
									solveMethod: $author$project$Pages$PowerTimeEnergyCalculator$DurationSolve
								});
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{activeField: $author$project$Pages$PowerTimeEnergyCalculator$NoActiveField, formStatus: $author$project$Pages$PowerTimeEnergyCalculator$Valid}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve = {$: 'CurrentSolve'};
var $author$project$Pages$VoltageCurrentPowerCalculator$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve = {$: 'VoltageSolve'};
var $author$project$Pages$VoltageCurrentPowerCalculator$calculateCurrent = F2(
	function (_v0, _v1) {
		var wattsValue = _v0.a;
		var voltsValue = _v1.a;
		return $author$project$Units$Electricity$Amps(wattsValue / voltsValue);
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$calculateVoltage = F2(
	function (_v0, _v1) {
		var wattsValue = _v0.a;
		var ampsValue = _v1.a;
		return $author$project$Units$Electricity$Volts(wattsValue / ampsValue);
	});
var $author$project$Units$Electricity$floatToCurrent = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$Amps(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $author$project$Units$Electricity$floatToVoltage = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$Volts(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$updateCurrent = F2(
	function (model, current) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'PowerSolve':
				return _Utils_update(
					model,
					{
						current: current,
						power: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculatePower, model.voltage, current)
					});
			case 'VoltageSolve':
				return _Utils_update(
					model,
					{
						current: current,
						voltage: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateVoltage, model.power, current)
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$updatePower = F2(
	function (model, power) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'VoltageSolve':
				return _Utils_update(
					model,
					{
						power: power,
						voltage: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateVoltage, power, model.current)
					});
			case 'CurrentSolve':
				return _Utils_update(
					model,
					{
						current: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateCurrent, power, model.voltage),
						power: power
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$updateVoltage = F2(
	function (model, voltage) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'PowerSolve':
				return _Utils_update(
					model,
					{
						power: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculatePower, voltage, model.current),
						voltage: voltage
					});
			case 'CurrentSolve':
				return _Utils_update(
					model,
					{
						current: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateCurrent, model.power, voltage),
						voltage: voltage
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSolveMethod':
				var solveMethod = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{solveMethod: solveMethod}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateField':
				var field = msg.a;
				var typedValue = msg.b;
				var _v1 = $author$project$Units$Number$numberStringToFloat(typedValue);
				if (_v1.$ === 'Just') {
					var floatValue = _v1.a;
					var newModel = function () {
						switch (field.$) {
							case 'WattsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updatePower,
									model,
									$author$project$Units$Electricity$Watts(floatValue));
							case 'KilowattsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegawattsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Mega, floatValue));
							case 'GigawattsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updatePower,
									model,
									A2($author$project$Units$Electricity$floatToPower, $author$project$Units$Metric$Giga, floatValue));
							case 'VoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateVoltage,
									model,
									$author$project$Units$Electricity$Volts(floatValue));
							case 'KilovoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegavoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Mega, floatValue));
							case 'GigavoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Giga, floatValue));
							case 'AmpsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateCurrent,
									model,
									$author$project$Units$Electricity$Amps(floatValue));
							case 'KiloampsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegaampsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Mega, floatValue));
							case 'GigaampsField':
								return A2(
									$author$project$Pages$VoltageCurrentPowerCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Giga, floatValue));
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: field, formStatus: $author$project$Pages$VoltageCurrentPowerCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: field,
								formStatus: $author$project$Pages$VoltageCurrentPowerCalculator$Invalid('must be a number'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var example = msg.a;
				var newModel = function () {
					switch (example.$) {
						case 'VoltageExample':
							var power = example.a;
							var current = example.b;
							return _Utils_update(
								model,
								{
									current: current,
									power: power,
									solveMethod: $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve,
									voltage: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateVoltage, power, current)
								});
						case 'CurrentExample':
							var power = example.a;
							var voltage = example.b;
							return _Utils_update(
								model,
								{
									current: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculateCurrent, power, voltage),
									power: power,
									solveMethod: $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve,
									voltage: voltage
								});
						default:
							var voltage = example.a;
							var current = example.b;
							return _Utils_update(
								model,
								{
									current: current,
									power: A2($author$project$Pages$VoltageCurrentPowerCalculator$calculatePower, voltage, current),
									solveMethod: $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve,
									voltage: voltage
								});
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{activeField: $author$project$Pages$VoltageCurrentPowerCalculator$NoActiveField, formStatus: $author$project$Pages$VoltageCurrentPowerCalculator$Valid}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve = {$: 'CurrentSolve'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve = {$: 'VoltageSolve'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$calculateCurrent = F2(
	function (_v0, _v1) {
		var ohmsValue = _v0.a;
		var voltsValue = _v1.a;
		return $author$project$Units$Electricity$Amps(voltsValue / ohmsValue);
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$calculateVoltage = F2(
	function (_v0, _v1) {
		var ohmsValue = _v0.a;
		var ampsValue = _v1.a;
		return $author$project$Units$Electricity$Volts(ohmsValue * ampsValue);
	});
var $author$project$Units$Electricity$floatToResistance = F2(
	function (oldPrefix, value) {
		return $author$project$Units$Electricity$Ohms(
			A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, oldPrefix, $author$project$Units$Metric$Base));
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$updateCurrent = F2(
	function (model, current) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'ResistanceSolve':
				return _Utils_update(
					model,
					{
						current: current,
						resistance: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateResistance, model.voltage, current)
					});
			case 'VoltageSolve':
				return _Utils_update(
					model,
					{
						current: current,
						voltage: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateVoltage, model.resistance, current)
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$updateResistance = F2(
	function (model, resistance) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'VoltageSolve':
				return _Utils_update(
					model,
					{
						resistance: resistance,
						voltage: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateVoltage, resistance, model.current)
					});
			case 'CurrentSolve':
				return _Utils_update(
					model,
					{
						current: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateCurrent, resistance, model.voltage),
						resistance: resistance
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$updateVoltage = F2(
	function (model, voltage) {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'ResistanceSolve':
				return _Utils_update(
					model,
					{
						resistance: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateResistance, voltage, model.current),
						voltage: voltage
					});
			case 'CurrentSolve':
				return _Utils_update(
					model,
					{
						current: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateCurrent, model.resistance, voltage),
						voltage: voltage
					});
			default:
				return model;
		}
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'SetSolveMethod':
				var solveMethod = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{solveMethod: solveMethod}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateField':
				var field = msg.a;
				var typedValue = msg.b;
				var _v1 = $author$project$Units$Number$numberStringToFloat(typedValue);
				if (_v1.$ === 'Just') {
					var floatValue = _v1.a;
					var newModel = function () {
						switch (field.$) {
							case 'OhmsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateResistance,
									model,
									$author$project$Units$Electricity$Ohms(floatValue));
							case 'KiloohmsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateResistance,
									model,
									A2($author$project$Units$Electricity$floatToResistance, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegaohmsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateResistance,
									model,
									A2($author$project$Units$Electricity$floatToResistance, $author$project$Units$Metric$Mega, floatValue));
							case 'GigaohmsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateResistance,
									model,
									A2($author$project$Units$Electricity$floatToResistance, $author$project$Units$Metric$Giga, floatValue));
							case 'VoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateVoltage,
									model,
									$author$project$Units$Electricity$Volts(floatValue));
							case 'KilovoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegavoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Mega, floatValue));
							case 'GigavoltsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateVoltage,
									model,
									A2($author$project$Units$Electricity$floatToVoltage, $author$project$Units$Metric$Giga, floatValue));
							case 'AmpsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateCurrent,
									model,
									$author$project$Units$Electricity$Amps(floatValue));
							case 'KiloampsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Kilo, floatValue));
							case 'MegaampsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Mega, floatValue));
							case 'GigaampsField':
								return A2(
									$author$project$Pages$VoltageCurrentResistanceCalculator$updateCurrent,
									model,
									A2($author$project$Units$Electricity$floatToCurrent, $author$project$Units$Metric$Giga, floatValue));
							default:
								return model;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							newModel,
							{activeField: field, formStatus: $author$project$Pages$VoltageCurrentResistanceCalculator$Valid, typedValue: typedValue}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								activeField: field,
								formStatus: $author$project$Pages$VoltageCurrentResistanceCalculator$Invalid('must be a number'),
								typedValue: typedValue
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var example = msg.a;
				var newModel = function () {
					switch (example.$) {
						case 'VoltageExample':
							var resistance = example.a;
							var current = example.b;
							return _Utils_update(
								model,
								{
									current: current,
									resistance: resistance,
									solveMethod: $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve,
									voltage: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateVoltage, resistance, current)
								});
						case 'CurrentExample':
							var resistance = example.a;
							var voltage = example.b;
							return _Utils_update(
								model,
								{
									current: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateCurrent, resistance, voltage),
									resistance: resistance,
									solveMethod: $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve,
									voltage: voltage
								});
						default:
							var voltage = example.a;
							var current = example.b;
							return _Utils_update(
								model,
								{
									current: current,
									resistance: A2($author$project$Pages$VoltageCurrentResistanceCalculator$calculateResistance, voltage, current),
									solveMethod: $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve,
									voltage: voltage
								});
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{activeField: $author$project$Pages$VoltageCurrentResistanceCalculator$NoActiveField, formStatus: $author$project$Pages$VoltageCurrentResistanceCalculator$Valid}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(msg, model.page);
		_v0$7:
		while (true) {
			switch (_v0.a.$) {
				case 'PowerTimeEnergyCalculatorMsg':
					if (_v0.b.$ === 'PowerTimeEnergyCalculator') {
						var subMsg = _v0.a.a;
						var pageModel = _v0.b.a;
						var _v1 = A2($author$project$Pages$PowerTimeEnergyCalculator$update, subMsg, pageModel);
						var updatedPageModel = _v1.a;
						var updatedCmd = _v1.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Main$PowerTimeEnergyCalculator(updatedPageModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$PowerTimeEnergyCalculatorMsg, updatedCmd));
					} else {
						break _v0$7;
					}
				case 'VoltageCurrentPowerCalculatorMsg':
					if (_v0.b.$ === 'VoltageCurrentPowerCalculator') {
						var subMsg = _v0.a.a;
						var pageModel = _v0.b.a;
						var _v2 = A2($author$project$Pages$VoltageCurrentPowerCalculator$update, subMsg, pageModel);
						var updatedPageModel = _v2.a;
						var updatedCmd = _v2.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Main$VoltageCurrentPowerCalculator(updatedPageModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$VoltageCurrentPowerCalculatorMsg, updatedCmd));
					} else {
						break _v0$7;
					}
				case 'VoltageCurrentResistanceCalculatorMsg':
					if (_v0.b.$ === 'VoltageCurrentResistanceCalculator') {
						var subMsg = _v0.a.a;
						var pageModel = _v0.b.a;
						var _v3 = A2($author$project$Pages$VoltageCurrentResistanceCalculator$update, subMsg, pageModel);
						var updatedPageModel = _v3.a;
						var updatedCmd = _v3.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Main$VoltageCurrentResistanceCalculator(updatedPageModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$VoltageCurrentResistanceCalculatorMsg, updatedCmd));
					} else {
						break _v0$7;
					}
				case 'EnergyCostCalculatorMsg':
					if (_v0.b.$ === 'EnergyCostCalculator') {
						var subMsg = _v0.a.a;
						var pageModel = _v0.b.a;
						var _v4 = A2($author$project$Pages$EnergyCostCalculator$update, subMsg, pageModel);
						var updatedPageModel = _v4.a;
						var updatedCmd = _v4.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Main$EnergyCostCalculator(updatedPageModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$EnergyCostCalculatorMsg, updatedCmd));
					} else {
						break _v0$7;
					}
				case 'FrequencyRevolutionsPolesCalculatorMsg':
					if (_v0.b.$ === 'FrequencyRpmPolesCalculator') {
						var subMsg = _v0.a.a;
						var pageModel = _v0.b.a;
						var _v5 = A2($author$project$Pages$FrequencyRpmPolesCalculator$update, subMsg, pageModel);
						var updatedPageModel = _v5.a;
						var updatedCmd = _v5.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									page: $author$project$Main$FrequencyRpmPolesCalculator(updatedPageModel)
								}),
							A2($elm$core$Platform$Cmd$map, $author$project$Main$FrequencyRevolutionsPolesCalculatorMsg, updatedCmd));
					} else {
						break _v0$7;
					}
				case 'LinkClicked':
					var urlRequest = _v0.a.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							A2(
								$elm$browser$Browser$Navigation$pushUrl,
								model.navKey,
								$elm$url$Url$toString(url)));
					} else {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							$elm$browser$Browser$Navigation$load(url));
					}
				default:
					var url = _v0.a.a;
					var newRoute = $author$project$Routes$parseUrl(url);
					return $author$project$Main$initCurrentPage(
						_Utils_Tuple2(
							_Utils_update(
								model,
								{route: newRoute}),
							$elm$core$Platform$Cmd$none));
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$notFoundView = A2(
	$elm$html$Html$h3,
	_List_Nil,
	_List_fromArray(
		[
			$elm$html$Html$text('Oops! The page you requested was not found!')
		]));
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Pages$EnergyCostCalculator$EnergyExample = F2(
	function (a, b) {
		return {$: 'EnergyExample', a: a, b: b};
	});
var $author$project$Pages$EnergyCostCalculator$GigawattHourCostField = {$: 'GigawattHourCostField'};
var $author$project$Pages$EnergyCostCalculator$GigawattHoursField = {$: 'GigawattHoursField'};
var $author$project$Pages$EnergyCostCalculator$KilowattHourCostField = {$: 'KilowattHourCostField'};
var $author$project$Pages$EnergyCostCalculator$KilowattHoursField = {$: 'KilowattHoursField'};
var $author$project$Pages$EnergyCostCalculator$MegawattHourCostField = {$: 'MegawattHourCostField'};
var $author$project$Pages$EnergyCostCalculator$MegawattHoursField = {$: 'MegawattHoursField'};
var $author$project$Pages$EnergyCostCalculator$SetExample = function (a) {
	return {$: 'SetExample', a: a};
};
var $author$project$Pages$EnergyCostCalculator$SetSolveMethod = function (a) {
	return {$: 'SetSolveMethod', a: a};
};
var $author$project$Pages$EnergyCostCalculator$TotalCostExample = F2(
	function (a, b) {
		return {$: 'TotalCostExample', a: a, b: b};
	});
var $author$project$Pages$EnergyCostCalculator$TotalCostField = {$: 'TotalCostField'};
var $author$project$Pages$EnergyCostCalculator$WattHourCostExample = F2(
	function (a, b) {
		return {$: 'WattHourCostExample', a: a, b: b};
	});
var $author$project$Pages$EnergyCostCalculator$WattHourCostField = {$: 'WattHourCostField'};
var $author$project$Pages$EnergyCostCalculator$WattHoursField = {$: 'WattHoursField'};
var $author$project$UI$cardBody = function (content) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('px-6 py-2')
			]),
		content);
};
var $author$project$UI$cardContainer = function (content) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('bg-gray-200 pb-6')
			]),
		content);
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$UI$cardHeader = F2(
	function (isActive, content) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mb-4 text-white px-4 py-2 sm:flex justify-between items-center uppercase'),
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('bg-green-800', isActive),
							_Utils_Tuple2('bg-gray-800', !isActive)
						]))
				]),
			content);
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$UI$cardHeaderToggleButton = F4(
	function (isActive, activeTitle, inactiveTitle, toMsg) {
		return isActive ? A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(activeTitle)
				])) : A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-sm'),
					$elm$html$Html$Events$onClick(toMsg)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(inactiveTitle)
				]));
	});
var $author$project$UI$cardTitle = function (title) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('font-bold text-2xl mb-1 sm:mb-0')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(title)
			]));
};
var $author$project$UI$cardsGrid = F2(
	function (columnCount, content) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('lg:grid grid-cols-3 gap-4')
				]),
			content);
	});
var $author$project$Units$Currency$currencyToFloat = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Units$Electricity$energyCostToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$ASC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$energyToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $cuducos$elm_format_number$FormatNumber$Locales$Min = function (a) {
	return {$: 'Min', a: a};
};
var $cuducos$elm_format_number$FormatNumber$Parser$FormattedNumber = F5(
	function (original, integers, decimals, prefix, suffix) {
		return {decimals: decimals, integers: integers, original: original, prefix: prefix, suffix: suffix};
	});
var $cuducos$elm_format_number$FormatNumber$Parser$Negative = {$: 'Negative'};
var $cuducos$elm_format_number$FormatNumber$Parser$Positive = {$: 'Positive'};
var $cuducos$elm_format_number$FormatNumber$Parser$Zero = {$: 'Zero'};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $cuducos$elm_format_number$FormatNumber$Parser$classify = function (formatted) {
	var onlyZeros = A2(
		$elm$core$String$all,
		function (_char) {
			return _Utils_eq(
				_char,
				_Utils_chr('0'));
		},
		$elm$core$String$concat(
			A2(
				$elm$core$List$append,
				formatted.integers,
				$elm$core$List$singleton(formatted.decimals))));
	return onlyZeros ? $cuducos$elm_format_number$FormatNumber$Parser$Zero : ((formatted.original < 0) ? $cuducos$elm_format_number$FormatNumber$Parser$Negative : $cuducos$elm_format_number$FormatNumber$Parser$Positive);
};
var $elm$core$String$filter = _String_filter;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $cuducos$elm_format_number$FormatNumber$Parser$addZerosToFit = F2(
	function (desiredLength, value) {
		var length = $elm$core$String$length(value);
		var missing = (_Utils_cmp(length, desiredLength) < 0) ? $elm$core$Basics$abs(desiredLength - length) : 0;
		return _Utils_ap(
			value,
			A2($elm$core$String$repeat, missing, '0'));
	});
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $cuducos$elm_format_number$FormatNumber$Parser$removeZeros = function (decimals) {
	return (A2($elm$core$String$right, 1, decimals) !== '0') ? decimals : $cuducos$elm_format_number$FormatNumber$Parser$removeZeros(
		A2($elm$core$String$dropRight, 1, decimals));
};
var $cuducos$elm_format_number$FormatNumber$Parser$getDecimals = F2(
	function (locale, digits) {
		var _v0 = locale.decimals;
		switch (_v0.$) {
			case 'Max':
				return $cuducos$elm_format_number$FormatNumber$Parser$removeZeros(digits);
			case 'Exact':
				return digits;
			default:
				var min = _v0.a;
				return A2($cuducos$elm_format_number$FormatNumber$Parser$addZerosToFit, min, digits);
		}
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				_Utils_chr('0'),
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				$elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					$elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _v0.a.a.valueOf()) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $cuducos$elm_format_number$FormatNumber$Parser$splitInParts = F2(
	function (locale, value) {
		var toString = function () {
			var _v1 = locale.decimals;
			switch (_v1.$) {
				case 'Max':
					var max = _v1.a;
					return $myrho$elm_round$Round$round(max);
				case 'Min':
					return $elm$core$String$fromFloat;
				default:
					var exact = _v1.a;
					return $myrho$elm_round$Round$round(exact);
			}
		}();
		var asList = A2(
			$elm$core$String$split,
			'.',
			toString(value));
		var decimals = function () {
			var _v0 = $elm$core$List$tail(asList);
			if (_v0.$ === 'Just') {
				var values = _v0.a;
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					$elm$core$List$head(values));
			} else {
				return '';
			}
		}();
		var integers = A2(
			$elm$core$Maybe$withDefault,
			'',
			$elm$core$List$head(asList));
		return _Utils_Tuple2(integers, decimals);
	});
var $cuducos$elm_format_number$FormatNumber$Parser$splitByIndian = function (integers) {
	var thousand = ($elm$core$String$length(integers) > 3) ? A2($elm$core$String$right, 3, integers) : integers;
	var reversedSplitHundreds = function (value) {
		return ($elm$core$String$length(value) > 2) ? A2(
			$elm$core$List$cons,
			A2($elm$core$String$right, 2, value),
			reversedSplitHundreds(
				A2($elm$core$String$dropRight, 2, value))) : ((!$elm$core$String$length(value)) ? _List_Nil : _List_fromArray(
			[value]));
	};
	return $elm$core$List$reverse(
		A2(
			$elm$core$List$cons,
			thousand,
			reversedSplitHundreds(
				A2($elm$core$String$dropRight, 3, integers))));
};
var $cuducos$elm_format_number$FormatNumber$Parser$splitByWestern = function (integers) {
	var reversedSplitThousands = function (value) {
		return ($elm$core$String$length(value) > 3) ? A2(
			$elm$core$List$cons,
			A2($elm$core$String$right, 3, value),
			reversedSplitThousands(
				A2($elm$core$String$dropRight, 3, value))) : _List_fromArray(
			[value]);
	};
	return $elm$core$List$reverse(
		reversedSplitThousands(integers));
};
var $cuducos$elm_format_number$FormatNumber$Parser$splitIntegers = F2(
	function (system, integers) {
		if (system.$ === 'Western') {
			return $cuducos$elm_format_number$FormatNumber$Parser$splitByWestern(
				A2($elm$core$String$filter, $elm$core$Char$isDigit, integers));
		} else {
			return $cuducos$elm_format_number$FormatNumber$Parser$splitByIndian(
				A2($elm$core$String$filter, $elm$core$Char$isDigit, integers));
		}
	});
var $cuducos$elm_format_number$FormatNumber$Parser$parse = F2(
	function (locale, original) {
		var parts = A2($cuducos$elm_format_number$FormatNumber$Parser$splitInParts, locale, original);
		var integers = A2(
			$cuducos$elm_format_number$FormatNumber$Parser$splitIntegers,
			locale.system,
			A2($elm$core$String$filter, $elm$core$Char$isDigit, parts.a));
		var decimals = A2($cuducos$elm_format_number$FormatNumber$Parser$getDecimals, locale, parts.b);
		var partial = A5($cuducos$elm_format_number$FormatNumber$Parser$FormattedNumber, original, integers, decimals, '', '');
		var _v0 = $cuducos$elm_format_number$FormatNumber$Parser$classify(partial);
		switch (_v0.$) {
			case 'Negative':
				return _Utils_update(
					partial,
					{prefix: locale.negativePrefix, suffix: locale.negativeSuffix});
			case 'Positive':
				return _Utils_update(
					partial,
					{prefix: locale.positivePrefix, suffix: locale.positiveSuffix});
			default:
				return _Utils_update(
					partial,
					{prefix: locale.zeroPrefix, suffix: locale.zeroSuffix});
		}
	});
var $cuducos$elm_format_number$FormatNumber$Stringfy$formatDecimals = F2(
	function (locale, decimals) {
		return (decimals === '') ? '' : _Utils_ap(locale.decimalSeparator, decimals);
	});
var $cuducos$elm_format_number$FormatNumber$Stringfy$stringfy = F2(
	function (locale, formatted) {
		var stringfyDecimals = $cuducos$elm_format_number$FormatNumber$Stringfy$formatDecimals(locale);
		var integers = A2($elm$core$String$join, locale.thousandSeparator, formatted.integers);
		var decimals = stringfyDecimals(formatted.decimals);
		return $elm$core$String$concat(
			_List_fromArray(
				[formatted.prefix, integers, decimals, formatted.suffix]));
	});
var $cuducos$elm_format_number$FormatNumber$format = F2(
	function (locale, number_) {
		return A2(
			$cuducos$elm_format_number$FormatNumber$Stringfy$stringfy,
			locale,
			A2($cuducos$elm_format_number$FormatNumber$Parser$parse, locale, number_));
	});
var $cuducos$elm_format_number$FormatNumber$Locales$Exact = function (a) {
	return {$: 'Exact', a: a};
};
var $cuducos$elm_format_number$FormatNumber$Locales$Western = {$: 'Western'};
var $cuducos$elm_format_number$FormatNumber$Locales$base = {
	decimalSeparator: '.',
	decimals: $cuducos$elm_format_number$FormatNumber$Locales$Min(0),
	negativePrefix: '???',
	negativeSuffix: '',
	positivePrefix: '',
	positiveSuffix: '',
	system: $cuducos$elm_format_number$FormatNumber$Locales$Western,
	thousandSeparator: '',
	zeroPrefix: '',
	zeroSuffix: ''
};
var $cuducos$elm_format_number$FormatNumber$Locales$usLocale = _Utils_update(
	$cuducos$elm_format_number$FormatNumber$Locales$base,
	{
		decimals: $cuducos$elm_format_number$FormatNumber$Locales$Exact(2),
		thousandSeparator: ','
	});
var $author$project$Units$Currency$formatFloatAsCurrency = function (value) {
	return '$' + A2(
		$cuducos$elm_format_number$FormatNumber$format,
		_Utils_update(
			$cuducos$elm_format_number$FormatNumber$Locales$usLocale,
			{
				decimals: $cuducos$elm_format_number$FormatNumber$Locales$Min(2)
			}),
		value);
};
var $author$project$Units$Currency$formatCurrency = function (currency) {
	return $author$project$Units$Currency$formatFloatAsCurrency(
		$author$project$Units$Currency$currencyToFloat(currency));
};
var $cuducos$elm_format_number$FormatNumber$Locales$Max = function (a) {
	return {$: 'Max', a: a};
};
var $author$project$Units$Number$formatFloat = function (value) {
	return A2(
		$cuducos$elm_format_number$FormatNumber$format,
		_Utils_update(
			$cuducos$elm_format_number$FormatNumber$Locales$usLocale,
			{
				decimals: $cuducos$elm_format_number$FormatNumber$Locales$Max(6)
			}),
		value);
};
var $author$project$Units$Metric$prefixToLabel = function (prefix) {
	switch (prefix.$) {
		case 'Base':
			return '';
		case 'Kilo':
			return 'k';
		case 'Mega':
			return 'm';
		default:
			return 'g';
	}
};
var $author$project$Units$Electricity$formatEnergy = F2(
	function (newPrefix, wattHours) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$energyToFloat, newPrefix, wattHours)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'Wh'));
	});
var $author$project$Units$Electricity$formatEnergyCost = F2(
	function (newPrefix, wattHourCost) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$energyCostToFloat, newPrefix, wattHourCost)) + ('/' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'Wh'));
	});
var $author$project$UI$pageHeader = function (content) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('text-xl sm:text-2xl md:text-3xl font-bold mb-4 text-center w-full p-2')
			]),
		content);
};
var $author$project$Pages$EnergyCostCalculator$UpdateField = F2(
	function (a, b) {
		return {$: 'UpdateField', a: a, b: b};
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Forms$renderError = function (error) {
	return A2(
		$elm$html$Html$label,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('label')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('label-text text-error')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(error)
					]))
			]));
};
var $author$project$Forms$renderHint = function (hintText) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('label-text ml-2 mt-1')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(hintText)
			]));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Forms$formControl = F6(
	function (labelFor, valueFor, errors, hint, isDisabled, toMsg) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('form-control mb-2')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('label font-bold')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('label-text text-gray-700')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(labelFor)
								]))
						])),
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('text'),
							$elm$html$Html$Attributes$placeholder(labelFor),
							$elm$html$Html$Attributes$value(valueFor),
							$elm$html$Html$Attributes$disabled(isDisabled),
							$elm$html$Html$Attributes$class('input input-bordered'),
							$elm$html$Html$Events$onInput(toMsg)
						]),
					_List_Nil),
					$author$project$Forms$renderHint(hint),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2($elm$core$List$map, $author$project$Forms$renderError, errors))
				]));
	});
var $author$project$Pages$EnergyCostCalculator$renderField = F6(
	function (model, field, label, forInput, forHint, solveMethod) {
		var _v0 = function () {
			var _v1 = model.formStatus;
			if (_v1.$ === 'Valid') {
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(model.typedValue, _List_Nil, forHint) : _Utils_Tuple3(
					$elm$core$String$fromFloat(forInput),
					_List_Nil,
					forHint);
			} else {
				var errorMsg = _v1.a;
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_fromArray(
						[errorMsg]),
					'') : _Utils_Tuple3(
					$elm$core$String$fromFloat(forInput),
					_List_Nil,
					forHint);
			}
		}();
		var value = _v0.a;
		var errors = _v0.b;
		var hint = _v0.c;
		return A6(
			$author$project$Forms$formControl,
			label,
			value,
			errors,
			hint,
			_Utils_eq(model.solveMethod, solveMethod),
			$author$project$Pages$EnergyCostCalculator$UpdateField(field));
	});
var $author$project$Pages$EnergyCostCalculator$renderMetricField = F8(
	function (model, field, unit, prefix, label, inputFn, hintFn, solveMethod) {
		return A6(
			$author$project$Pages$EnergyCostCalculator$renderField,
			model,
			field,
			label,
			A2(inputFn, prefix, unit),
			A2(hintFn, prefix, unit),
			solveMethod);
	});
var $author$project$UI$resourceLink = F2(
	function (title, toMsg) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('text-left text-blue-500 mb-2'),
							$elm$html$Html$Events$onClick(toMsg)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						]))
				]));
	});
var $author$project$UI$resourcesContainer = F3(
	function (title, subhead, content) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mt-8 text-lg align-left')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('font-bold')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('mb-2')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(subhead)
						])),
					A2($elm$html$Html$div, _List_Nil, content)
				]));
	});
var $author$project$Pages$EnergyCostCalculator$view = function (model) {
	var altClass = 'text-gray-400 mx-2';
	var formula = function () {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'EnergySolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Total Cost'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Energy Cost'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Energy')
					]);
			case 'EnergyCostSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Total Cost'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Energy'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Energy Cost')
					]);
			default:
				return _List_fromArray(
					[
						$elm$html$Html$text('Energy'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('x')
							])),
						$elm$html$Html$text('Energy Cost'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Total Cost')
					]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$UI$pageHeader(formula),
				A2(
				$author$project$UI$cardsGrid,
				3,
				_List_fromArray(
					[
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$EnergySolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Energy'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$EnergySolve),
										'Solving for Energy',
										'Solve for Energy',
										$author$project$Pages$EnergyCostCalculator$SetSolveMethod($author$project$Pages$EnergyCostCalculator$EnergySolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$WattHoursField, model.energy, $author$project$Units$Metric$Base, 'Watt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$EnergyCostCalculator$EnergySolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$KilowattHoursField, model.energy, $author$project$Units$Metric$Kilo, 'Kilowatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$EnergyCostCalculator$EnergySolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$MegawattHoursField, model.energy, $author$project$Units$Metric$Mega, 'Megawatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$EnergyCostCalculator$EnergySolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$GigawattHoursField, model.energy, $author$project$Units$Metric$Giga, 'Gigawatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$EnergyCostCalculator$EnergySolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Energy Cost'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve),
										'Solving for Energy Cost',
										'Solve for Energy Cost',
										$author$project$Pages$EnergyCostCalculator$SetSolveMethod($author$project$Pages$EnergyCostCalculator$EnergyCostSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$WattHourCostField, model.energyCost, $author$project$Units$Metric$Base, 'Cost Per Watt Hour', $author$project$Units$Electricity$energyCostToFloat, $author$project$Units$Electricity$formatEnergyCost, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$KilowattHourCostField, model.energyCost, $author$project$Units$Metric$Kilo, 'Cost Per Kilowatt Hour', $author$project$Units$Electricity$energyCostToFloat, $author$project$Units$Electricity$formatEnergyCost, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$MegawattHourCostField, model.energyCost, $author$project$Units$Metric$Mega, 'Cost Per Megawatt Hour', $author$project$Units$Electricity$energyCostToFloat, $author$project$Units$Electricity$formatEnergyCost, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve),
										A8($author$project$Pages$EnergyCostCalculator$renderMetricField, model, $author$project$Pages$EnergyCostCalculator$GigawattHourCostField, model.energyCost, $author$project$Units$Metric$Giga, 'Cost Per Gigawatt Hour', $author$project$Units$Electricity$energyCostToFloat, $author$project$Units$Electricity$formatEnergyCost, $author$project$Pages$EnergyCostCalculator$EnergyCostSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$TotalCostSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Total Cost'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$EnergyCostCalculator$TotalCostSolve),
										'Solving for Total Cost',
										'Solve for Total Cost',
										$author$project$Pages$EnergyCostCalculator$SetSolveMethod($author$project$Pages$EnergyCostCalculator$TotalCostSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A6(
										$author$project$Pages$EnergyCostCalculator$renderField,
										model,
										$author$project$Pages$EnergyCostCalculator$TotalCostField,
										'Total Cost',
										$author$project$Units$Currency$currencyToFloat(model.totalCost),
										$author$project$Units$Currency$formatCurrency(model.totalCost),
										$author$project$Pages$EnergyCostCalculator$TotalCostSolve)
									]))
							]))
					])),
				A3(
				$author$project$UI$resourcesContainer,
				'Examples',
				'Click the questions below to auto-fill the form with the solution:',
				_List_fromArray(
					[
						A2(
						$author$project$UI$resourceLink,
						'If the cost of energy is 12 cents per kilowatt hour, how many kilowatt hours would your central air need to use to reach a total cost of $50?',
						$author$project$Pages$EnergyCostCalculator$SetExample(
							A2(
								$author$project$Pages$EnergyCostCalculator$EnergyExample,
								$author$project$Units$Currency$Currency(50),
								A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Kilo, 0.12)))),
						A2(
						$author$project$UI$resourceLink,
						'If your home typically uses 900 kilowatt hours of energy per month, what would be the cost per kilowatt in order to get your total bill below $100?',
						$author$project$Pages$EnergyCostCalculator$SetExample(
							A2(
								$author$project$Pages$EnergyCostCalculator$WattHourCostExample,
								$author$project$Units$Currency$Currency(99.99),
								A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, 900)))),
						A2(
						$author$project$UI$resourceLink,
						'The average US home uses 30kWh of energy per day. If energy costs 12 cents per kilowatt hour, what would be the daily cost?',
						$author$project$Pages$EnergyCostCalculator$SetExample(
							A2(
								$author$project$Pages$EnergyCostCalculator$TotalCostExample,
								A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, 30),
								A2($author$project$Units$Electricity$floatToEnergyCost, $author$project$Units$Metric$Kilo, 0.12))))
					]))
			]));
};
var $author$project$Pages$FrequencyRpmPolesCalculator$FrequencyExample = F2(
	function (a, b) {
		return {$: 'FrequencyExample', a: a, b: b};
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$PolesExample = F2(
	function (a, b) {
		return {$: 'PolesExample', a: a, b: b};
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$RpmExample = F2(
	function (a, b) {
		return {$: 'RpmExample', a: a, b: b};
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$SetExample = function (a) {
	return {$: 'SetExample', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$SetSolveMethod = function (a) {
	return {$: 'SetSolveMethod', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$UpdateHertzField = function (a) {
	return {$: 'UpdateHertzField', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$UpdatePolesField = function (a) {
	return {$: 'UpdatePolesField', a: a};
};
var $author$project$Pages$FrequencyRpmPolesCalculator$UpdateRpmField = function (a) {
	return {$: 'UpdateRpmField', a: a};
};
var $author$project$Units$Electricity$frequencyToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$formatFrequency = F2(
	function (newPrefix, hertz) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$frequencyToFloat, newPrefix, hertz)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'Hz'));
	});
var $author$project$Units$Number$formatInt = function (value) {
	return A2(
		$cuducos$elm_format_number$FormatNumber$format,
		_Utils_update(
			$cuducos$elm_format_number$FormatNumber$Locales$usLocale,
			{
				decimals: $cuducos$elm_format_number$FormatNumber$Locales$Exact(0)
			}),
		value);
};
var $author$project$Pages$FrequencyRpmPolesCalculator$formatPoles = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatInt(value) + ' poles';
};
var $author$project$Pages$FrequencyRpmPolesCalculator$formatRpm = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatInt(value) + ' rpm';
};
var $author$project$Pages$FrequencyRpmPolesCalculator$polesToInt = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Pages$FrequencyRpmPolesCalculator$renderField = F7(
	function (model, field, label, forInput, forHint, solveMethod, toMsg) {
		var _v0 = function () {
			var _v1 = model.formStatus;
			if (_v1.$ === 'Valid') {
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(model.typedValue, _List_Nil, forHint) : _Utils_Tuple3(forInput, _List_Nil, forHint);
			} else {
				var errorMsg = _v1.a;
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_fromArray(
						[errorMsg]),
					'') : _Utils_Tuple3(forInput, _List_Nil, forHint);
			}
		}();
		var value = _v0.a;
		var errors = _v0.b;
		var hint = _v0.c;
		return A6(
			$author$project$Forms$formControl,
			label,
			value,
			errors,
			hint,
			_Utils_eq(model.solveMethod, solveMethod),
			toMsg);
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$renderMetricField = F9(
	function (model, field, unit, prefix, label, inputFn, hintFn, solveMethod, toMsg) {
		return A7(
			$author$project$Pages$FrequencyRpmPolesCalculator$renderField,
			model,
			field,
			label,
			$elm$core$String$fromFloat(
				A2(inputFn, prefix, unit)),
			A2(hintFn, prefix, unit),
			solveMethod,
			toMsg);
	});
var $author$project$Pages$FrequencyRpmPolesCalculator$rpmToInt = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Pages$FrequencyRpmPolesCalculator$view = function (model) {
	var altClass = 'text-gray-400 mx-2';
	var formula = function () {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'FrequencySolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('RPMs'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Poles'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Frequency')
					]);
			case 'PolesSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('RPMs'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Frequency'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Poles')
					]);
			default:
				return _List_fromArray(
					[
						$elm$html$Html$text('Frequency'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('x')
							])),
						$elm$html$Html$text('Poles'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('RPMs')
					]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$UI$pageHeader(formula),
				A2(
				$author$project$UI$cardsGrid,
				3,
				_List_fromArray(
					[
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Frequency'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve),
										'Solving for Frequency',
										'Solve for Frequency',
										$author$project$Pages$FrequencyRpmPolesCalculator$SetSolveMethod($author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A9($author$project$Pages$FrequencyRpmPolesCalculator$renderMetricField, model, $author$project$Pages$FrequencyRpmPolesCalculator$HertzField, model.frequency, $author$project$Units$Metric$Base, 'Hertz', $author$project$Units$Electricity$frequencyToFloat, $author$project$Units$Electricity$formatFrequency, $author$project$Pages$FrequencyRpmPolesCalculator$FrequencySolve, $author$project$Pages$FrequencyRpmPolesCalculator$UpdateHertzField)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Rotor Poles'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve),
										'Solving for Rotor Poles',
										'Solve for Rotor Poles',
										$author$project$Pages$FrequencyRpmPolesCalculator$SetSolveMethod($author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A7(
										$author$project$Pages$FrequencyRpmPolesCalculator$renderField,
										model,
										$author$project$Pages$FrequencyRpmPolesCalculator$PolesField,
										'# Poles',
										$elm$core$String$fromInt(
											$author$project$Pages$FrequencyRpmPolesCalculator$polesToInt(model.poles)),
										$author$project$Pages$FrequencyRpmPolesCalculator$formatPoles(model.poles),
										$author$project$Pages$FrequencyRpmPolesCalculator$PolesSolve,
										$author$project$Pages$FrequencyRpmPolesCalculator$UpdatePolesField)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('RPMs'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve),
										'Solving for RPMs',
										'Solve for RPMs',
										$author$project$Pages$FrequencyRpmPolesCalculator$SetSolveMethod($author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A7(
										$author$project$Pages$FrequencyRpmPolesCalculator$renderField,
										model,
										$author$project$Pages$FrequencyRpmPolesCalculator$RpmField,
										'RPMs',
										$elm$core$String$fromInt(
											$author$project$Pages$FrequencyRpmPolesCalculator$rpmToInt(model.rpm)),
										$author$project$Pages$FrequencyRpmPolesCalculator$formatRpm(model.rpm),
										$author$project$Pages$FrequencyRpmPolesCalculator$RpmSolve,
										$author$project$Pages$FrequencyRpmPolesCalculator$UpdateRpmField)
									]))
							]))
					])),
				A3(
				$author$project$UI$resourcesContainer,
				'Examples',
				'Click the questions below to auto-fill the form with the solution:',
				_List_fromArray(
					[
						A2(
						$author$project$UI$resourceLink,
						'For a frequency of 60Hz with 2 rotor poles what would be the RPMs?',
						$author$project$Pages$FrequencyRpmPolesCalculator$SetExample(
							A2(
								$author$project$Pages$FrequencyRpmPolesCalculator$RpmExample,
								$author$project$Units$Electricity$Hertz(60),
								$author$project$Pages$FrequencyRpmPolesCalculator$Poles(2)))),
						A2(
						$author$project$UI$resourceLink,
						'At 3000 RPMs with 2 rotor poles, what is the frequency?',
						$author$project$Pages$FrequencyRpmPolesCalculator$SetExample(
							A2(
								$author$project$Pages$FrequencyRpmPolesCalculator$FrequencyExample,
								$author$project$Pages$FrequencyRpmPolesCalculator$Rpm(3000),
								$author$project$Pages$FrequencyRpmPolesCalculator$Poles(2)))),
						A2(
						$author$project$UI$resourceLink,
						'How many rotor poles would there be for 7200 RPMs at a frequency of 60 Hz?',
						$author$project$Pages$FrequencyRpmPolesCalculator$SetExample(
							A2(
								$author$project$Pages$FrequencyRpmPolesCalculator$PolesExample,
								$author$project$Units$Electricity$Hertz(60),
								$author$project$Pages$FrequencyRpmPolesCalculator$Rpm(7200))))
					]))
			]));
};
var $author$project$Pages$PowerTimeEnergyCalculator$DaysField = {$: 'DaysField'};
var $author$project$Pages$PowerTimeEnergyCalculator$DurationExample = F2(
	function (a, b) {
		return {$: 'DurationExample', a: a, b: b};
	});
var $author$project$Pages$PowerTimeEnergyCalculator$EnergyExample = F2(
	function (a, b) {
		return {$: 'EnergyExample', a: a, b: b};
	});
var $author$project$Pages$PowerTimeEnergyCalculator$GigawattHoursField = {$: 'GigawattHoursField'};
var $author$project$Pages$PowerTimeEnergyCalculator$GigawattsField = {$: 'GigawattsField'};
var $author$project$Pages$PowerTimeEnergyCalculator$HoursField = {$: 'HoursField'};
var $author$project$Pages$PowerTimeEnergyCalculator$KilowattHoursField = {$: 'KilowattHoursField'};
var $author$project$Pages$PowerTimeEnergyCalculator$KilowattsField = {$: 'KilowattsField'};
var $author$project$Pages$PowerTimeEnergyCalculator$MegawattHoursField = {$: 'MegawattHoursField'};
var $author$project$Pages$PowerTimeEnergyCalculator$MegawattsField = {$: 'MegawattsField'};
var $author$project$Pages$PowerTimeEnergyCalculator$MinutesField = {$: 'MinutesField'};
var $author$project$Pages$PowerTimeEnergyCalculator$PowerExample = F2(
	function (a, b) {
		return {$: 'PowerExample', a: a, b: b};
	});
var $author$project$Pages$PowerTimeEnergyCalculator$SecondsField = {$: 'SecondsField'};
var $author$project$Pages$PowerTimeEnergyCalculator$SetExample = function (a) {
	return {$: 'SetExample', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$SetSolveMethod = function (a) {
	return {$: 'SetSolveMethod', a: a};
};
var $author$project$Pages$PowerTimeEnergyCalculator$WattHoursField = {$: 'WattHoursField'};
var $author$project$Pages$PowerTimeEnergyCalculator$WattsField = {$: 'WattsField'};
var $author$project$Units$Time$daysToFloat = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Units$Time$formatDays = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatFloat(value) + ' days';
};
var $author$project$Units$Time$formatHours = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatFloat(value) + ' hrs';
};
var $author$project$Units$Time$formatMinutes = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatFloat(value) + ' mins';
};
var $author$project$Units$Electricity$powerToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$formatPower = F2(
	function (newPrefix, watts) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$powerToFloat, newPrefix, watts)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'W'));
	});
var $author$project$Units$Time$formatSeconds = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Number$formatFloat(value) + ' secs';
};
var $author$project$Units$Time$minutesToFloat = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Pages$PowerTimeEnergyCalculator$UpdateField = F2(
	function (a, b) {
		return {$: 'UpdateField', a: a, b: b};
	});
var $author$project$Pages$PowerTimeEnergyCalculator$renderField = F6(
	function (model, field, label, forInput, forHint, solveMethod) {
		var _v0 = function () {
			var _v1 = model.formStatus;
			if (_v1.$ === 'Valid') {
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(model.typedValue, _List_Nil, forHint) : _Utils_Tuple3(
					$elm$core$String$fromFloat(forInput),
					_List_Nil,
					forHint);
			} else {
				var errorMsg = _v1.a;
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_fromArray(
						[errorMsg]),
					'') : _Utils_Tuple3(
					$elm$core$String$fromFloat(forInput),
					_List_Nil,
					forHint);
			}
		}();
		var value = _v0.a;
		var errors = _v0.b;
		var hint = _v0.c;
		return A6(
			$author$project$Forms$formControl,
			label,
			value,
			errors,
			hint,
			_Utils_eq(model.solveMethod, solveMethod),
			$author$project$Pages$PowerTimeEnergyCalculator$UpdateField(field));
	});
var $author$project$Pages$PowerTimeEnergyCalculator$renderMetricField = F8(
	function (model, field, unit, prefix, label, inputFn, hintFn, solveMethod) {
		return A6(
			$author$project$Pages$PowerTimeEnergyCalculator$renderField,
			model,
			field,
			label,
			A2(inputFn, prefix, unit),
			A2(hintFn, prefix, unit),
			solveMethod);
	});
var $author$project$Units$Time$secondsToDays = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Days(((value / 60) / 60) / 24);
};
var $author$project$Units$Time$secondsToFloat = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Units$Time$secondsToMinutes = function (_v0) {
	var value = _v0.a;
	return $author$project$Units$Time$Minutes(value / 60);
};
var $author$project$Pages$PowerTimeEnergyCalculator$view = function (model) {
	var altClass = 'text-gray-400 mx-2';
	var formula = function () {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'PowerSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Energy'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Time'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Power')
					]);
			case 'DurationSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Energy'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Power'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Time')
					]);
			default:
				return _List_fromArray(
					[
						$elm$html$Html$text('Power'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('x')
							])),
						$elm$html$Html$text('Time'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Energy')
					]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$UI$pageHeader(formula),
				A2(
				$author$project$UI$cardsGrid,
				3,
				_List_fromArray(
					[
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Power'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve),
										'Solving for Power',
										'Solve for Power',
										$author$project$Pages$PowerTimeEnergyCalculator$SetSolveMethod($author$project$Pages$PowerTimeEnergyCalculator$PowerSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$WattsField, model.power, $author$project$Units$Metric$Base, 'Watts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$KilowattsField, model.power, $author$project$Units$Metric$Kilo, 'Kilowatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$MegawattsField, model.power, $author$project$Units$Metric$Mega, 'Megawatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$GigawattsField, model.power, $author$project$Units$Metric$Giga, 'Gigawatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$PowerTimeEnergyCalculator$PowerSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$DurationSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Time'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$DurationSolve),
										'Solving for Time',
										'Solve for Time',
										$author$project$Pages$PowerTimeEnergyCalculator$SetSolveMethod($author$project$Pages$PowerTimeEnergyCalculator$DurationSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A6(
										$author$project$Pages$PowerTimeEnergyCalculator$renderField,
										model,
										$author$project$Pages$PowerTimeEnergyCalculator$SecondsField,
										'Seconds',
										$author$project$Units$Time$secondsToFloat(model.duration),
										$author$project$Units$Time$formatSeconds(model.duration),
										$author$project$Pages$PowerTimeEnergyCalculator$DurationSolve),
										A6(
										$author$project$Pages$PowerTimeEnergyCalculator$renderField,
										model,
										$author$project$Pages$PowerTimeEnergyCalculator$MinutesField,
										'Minutes',
										$author$project$Units$Time$minutesToFloat(
											$author$project$Units$Time$secondsToMinutes(model.duration)),
										$author$project$Units$Time$formatMinutes(
											$author$project$Units$Time$secondsToMinutes(model.duration)),
										$author$project$Pages$PowerTimeEnergyCalculator$DurationSolve),
										A6(
										$author$project$Pages$PowerTimeEnergyCalculator$renderField,
										model,
										$author$project$Pages$PowerTimeEnergyCalculator$HoursField,
										'Hours',
										$author$project$Units$Time$hoursToFloat(
											$author$project$Units$Time$secondsToHours(model.duration)),
										$author$project$Units$Time$formatHours(
											$author$project$Units$Time$secondsToHours(model.duration)),
										$author$project$Pages$PowerTimeEnergyCalculator$DurationSolve),
										A6(
										$author$project$Pages$PowerTimeEnergyCalculator$renderField,
										model,
										$author$project$Pages$PowerTimeEnergyCalculator$DaysField,
										'Days',
										$author$project$Units$Time$daysToFloat(
											$author$project$Units$Time$secondsToDays(model.duration)),
										$author$project$Units$Time$formatDays(
											$author$project$Units$Time$secondsToDays(model.duration)),
										$author$project$Pages$PowerTimeEnergyCalculator$DurationSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Energy'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve),
										'Solving for Energy',
										'Solve for Energy',
										$author$project$Pages$PowerTimeEnergyCalculator$SetSolveMethod($author$project$Pages$PowerTimeEnergyCalculator$EnergySolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$WattHoursField, model.energy, $author$project$Units$Metric$Base, 'Watt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$KilowattHoursField, model.energy, $author$project$Units$Metric$Kilo, 'Kilowatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$MegawattHoursField, model.energy, $author$project$Units$Metric$Mega, 'Megawatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve),
										A8($author$project$Pages$PowerTimeEnergyCalculator$renderMetricField, model, $author$project$Pages$PowerTimeEnergyCalculator$GigawattHoursField, model.energy, $author$project$Units$Metric$Giga, 'Gigawatt Hours', $author$project$Units$Electricity$energyToFloat, $author$project$Units$Electricity$formatEnergy, $author$project$Pages$PowerTimeEnergyCalculator$EnergySolve)
									]))
							]))
					])),
				A3(
				$author$project$UI$resourcesContainer,
				'Examples',
				'Click the questions below to auto-fill the form with the solution:',
				_List_fromArray(
					[
						A2(
						$author$project$UI$resourceLink,
						'The average US home uses 30kWh of energy per day. What is the average power?',
						$author$project$Pages$PowerTimeEnergyCalculator$SetExample(
							A2(
								$author$project$Pages$PowerTimeEnergyCalculator$PowerExample,
								A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, 30),
								$author$project$Units$Time$daysToSeconds(
									$author$project$Units$Time$Days(1))))),
						A2(
						$author$project$UI$resourceLink,
						'How long would it take for a central air unit running at 3000 watts to reach 30 kilowatt hours of energy?',
						$author$project$Pages$PowerTimeEnergyCalculator$SetExample(
							A2(
								$author$project$Pages$PowerTimeEnergyCalculator$DurationExample,
								$author$project$Units$Electricity$Watts(3000),
								A2($author$project$Units$Electricity$floatToEnergy, $author$project$Units$Metric$Kilo, 30)))),
						A2(
						$author$project$UI$resourceLink,
						'How much energy would be used by a 100w light bulb in 10 hours?',
						$author$project$Pages$PowerTimeEnergyCalculator$SetExample(
							A2(
								$author$project$Pages$PowerTimeEnergyCalculator$EnergyExample,
								$author$project$Units$Electricity$Watts(100),
								$author$project$Units$Time$hoursToSeconds(
									$author$project$Units$Time$Hours(10)))))
					]))
			]));
};
var $author$project$Pages$VoltageCurrentPowerCalculator$AmpsField = {$: 'AmpsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$CurrentExample = F2(
	function (a, b) {
		return {$: 'CurrentExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$GigaampsField = {$: 'GigaampsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$GigavoltsField = {$: 'GigavoltsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$GigawattsField = {$: 'GigawattsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$KiloampsField = {$: 'KiloampsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$KilovoltsField = {$: 'KilovoltsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$KilowattsField = {$: 'KilowattsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$MegaampsField = {$: 'MegaampsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$MegavoltsField = {$: 'MegavoltsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$MegawattsField = {$: 'MegawattsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$PowerExample = F2(
	function (a, b) {
		return {$: 'PowerExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$SetExample = function (a) {
	return {$: 'SetExample', a: a};
};
var $author$project$Pages$VoltageCurrentPowerCalculator$SetSolveMethod = function (a) {
	return {$: 'SetSolveMethod', a: a};
};
var $author$project$Pages$VoltageCurrentPowerCalculator$VoltageExample = F2(
	function (a, b) {
		return {$: 'VoltageExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$VoltsField = {$: 'VoltsField'};
var $author$project$Pages$VoltageCurrentPowerCalculator$WattsField = {$: 'WattsField'};
var $author$project$Units$Electricity$currentToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$formatCurrent = F2(
	function (newPrefix, amps) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$currentToFloat, newPrefix, amps)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'A'));
	});
var $author$project$Units$Electricity$voltageToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$formatVoltage = F2(
	function (newPrefix, volts) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$voltageToFloat, newPrefix, volts)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + 'V'));
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$UpdateField = F2(
	function (a, b) {
		return {$: 'UpdateField', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$renderField = F8(
	function (model, field, unit, prefix, label, inputFn, hintFn, solveMethod) {
		var _v0 = function () {
			var _v1 = model.formStatus;
			if (_v1.$ === 'Valid') {
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_Nil,
					A2(hintFn, prefix, unit)) : _Utils_Tuple3(
					$elm$core$String$fromFloat(
						A2(inputFn, prefix, unit)),
					_List_Nil,
					A2(hintFn, prefix, unit));
			} else {
				var errorMsg = _v1.a;
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_fromArray(
						[errorMsg]),
					'') : _Utils_Tuple3(
					$elm$core$String$fromFloat(
						A2(inputFn, prefix, unit)),
					_List_Nil,
					A2(hintFn, prefix, unit));
			}
		}();
		var value = _v0.a;
		var errors = _v0.b;
		var hint = _v0.c;
		return A6(
			$author$project$Forms$formControl,
			label,
			value,
			errors,
			hint,
			_Utils_eq(model.solveMethod, solveMethod),
			$author$project$Pages$VoltageCurrentPowerCalculator$UpdateField(field));
	});
var $author$project$Pages$VoltageCurrentPowerCalculator$view = function (model) {
	var altClass = 'text-gray-400 mx-2';
	var formula = function () {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'VoltageSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Power'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Current'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Voltage')
					]);
			case 'CurrentSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Power'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Voltage'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Current')
					]);
			default:
				return _List_fromArray(
					[
						$elm$html$Html$text('Voltage'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('x')
							])),
						$elm$html$Html$text('Current'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Power')
					]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$UI$pageHeader(formula),
				A2(
				$author$project$UI$cardsGrid,
				3,
				_List_fromArray(
					[
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Voltage'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve),
										'Solving for Voltage',
										'Solve for Voltage',
										$author$project$Pages$VoltageCurrentPowerCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$VoltsField, model.voltage, $author$project$Units$Metric$Base, 'Volts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$KilovoltsField, model.voltage, $author$project$Units$Metric$Kilo, 'Kilovolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$MegavoltsField, model.voltage, $author$project$Units$Metric$Mega, 'Megavolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$GigavoltsField, model.voltage, $author$project$Units$Metric$Giga, 'Gigavolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentPowerCalculator$VoltageSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Current'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve),
										'Solving for Current',
										'Solve for Current',
										$author$project$Pages$VoltageCurrentPowerCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$AmpsField, model.current, $author$project$Units$Metric$Base, 'Amps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$KiloampsField, model.current, $author$project$Units$Metric$Kilo, 'Kiloamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$MegaampsField, model.current, $author$project$Units$Metric$Mega, 'Megaamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$GigaampsField, model.current, $author$project$Units$Metric$Giga, 'Gigaamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentPowerCalculator$CurrentSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Power'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve),
										'Solving for Power',
										'Solve for Power',
										$author$project$Pages$VoltageCurrentPowerCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$WattsField, model.power, $author$project$Units$Metric$Base, 'Watts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$KilowattsField, model.power, $author$project$Units$Metric$Kilo, 'Kilowatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$MegawattsField, model.power, $author$project$Units$Metric$Mega, 'Megawatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve),
										A8($author$project$Pages$VoltageCurrentPowerCalculator$renderField, model, $author$project$Pages$VoltageCurrentPowerCalculator$GigawattsField, model.power, $author$project$Units$Metric$Giga, 'Gigawatts', $author$project$Units$Electricity$powerToFloat, $author$project$Units$Electricity$formatPower, $author$project$Pages$VoltageCurrentPowerCalculator$PowerSolve)
									]))
							]))
					])),
				A3(
				$author$project$UI$resourcesContainer,
				'Examples',
				'Click the questions below to auto-fill the form with the solution:',
				_List_fromArray(
					[
						A2(
						$author$project$UI$resourceLink,
						'What would be the voltage for a central air unit running at 3000 watts and using 12.5 amps?',
						$author$project$Pages$VoltageCurrentPowerCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentPowerCalculator$VoltageExample,
								$author$project$Units$Electricity$Watts(3000),
								$author$project$Units$Electricity$Amps(12.5)))),
						A2(
						$author$project$UI$resourceLink,
						'What would be the current for a 100 watt light bulb operating at 120 volts?',
						$author$project$Pages$VoltageCurrentPowerCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentPowerCalculator$CurrentExample,
								$author$project$Units$Electricity$Watts(100),
								$author$project$Units$Electricity$Volts(120)))),
						A2(
						$author$project$UI$resourceLink,
						'What would be the power for a refrigerator with a voltage of 240 volts and current of 2.5 amps',
						$author$project$Pages$VoltageCurrentPowerCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentPowerCalculator$PowerExample,
								$author$project$Units$Electricity$Volts(240),
								$author$project$Units$Electricity$Amps(2.5))))
					]))
			]));
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$AmpsField = {$: 'AmpsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentExample = F2(
	function (a, b) {
		return {$: 'CurrentExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$GigaampsField = {$: 'GigaampsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$GigaohmsField = {$: 'GigaohmsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$GigavoltsField = {$: 'GigavoltsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$KiloampsField = {$: 'KiloampsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$KiloohmsField = {$: 'KiloohmsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$KilovoltsField = {$: 'KilovoltsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$MegaampsField = {$: 'MegaampsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$MegaohmsField = {$: 'MegaohmsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$MegavoltsField = {$: 'MegavoltsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$OhmsField = {$: 'OhmsField'};
var $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceExample = F2(
	function (a, b) {
		return {$: 'ResistanceExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$SetExample = function (a) {
	return {$: 'SetExample', a: a};
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$SetSolveMethod = function (a) {
	return {$: 'SetSolveMethod', a: a};
};
var $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageExample = F2(
	function (a, b) {
		return {$: 'VoltageExample', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$VoltsField = {$: 'VoltsField'};
var $author$project$Units$Electricity$resistanceToFloat = F2(
	function (newPrefix, _v0) {
		var value = _v0.a;
		return A4($author$project$Units$Metric$convertPrefix, $author$project$Units$Metric$DESC, value, $author$project$Units$Metric$Base, newPrefix);
	});
var $author$project$Units$Electricity$formatResistance = F2(
	function (newPrefix, watts) {
		return $author$project$Units$Number$formatFloat(
			A2($author$project$Units$Electricity$resistanceToFloat, newPrefix, watts)) + (' ' + ($author$project$Units$Metric$prefixToLabel(newPrefix) + '??'));
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$UpdateField = F2(
	function (a, b) {
		return {$: 'UpdateField', a: a, b: b};
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$renderField = F8(
	function (model, field, unit, prefix, label, inputFn, hintFn, solveMethod) {
		var _v0 = function () {
			var _v1 = model.formStatus;
			if (_v1.$ === 'Valid') {
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_Nil,
					A2(hintFn, prefix, unit)) : _Utils_Tuple3(
					$elm$core$String$fromFloat(
						A2(inputFn, prefix, unit)),
					_List_Nil,
					A2(hintFn, prefix, unit));
			} else {
				var errorMsg = _v1.a;
				return _Utils_eq(model.activeField, field) ? _Utils_Tuple3(
					model.typedValue,
					_List_fromArray(
						[errorMsg]),
					'') : _Utils_Tuple3(
					$elm$core$String$fromFloat(
						A2(inputFn, prefix, unit)),
					_List_Nil,
					A2(hintFn, prefix, unit));
			}
		}();
		var value = _v0.a;
		var errors = _v0.b;
		var hint = _v0.c;
		return A6(
			$author$project$Forms$formControl,
			label,
			value,
			errors,
			hint,
			_Utils_eq(model.solveMethod, solveMethod),
			$author$project$Pages$VoltageCurrentResistanceCalculator$UpdateField(field));
	});
var $author$project$Pages$VoltageCurrentResistanceCalculator$view = function (model) {
	var altClass = 'text-gray-400 mx-2';
	var formula = function () {
		var _v0 = model.solveMethod;
		switch (_v0.$) {
			case 'VoltageSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Resistance'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('x')
							])),
						$elm$html$Html$text('Current'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Voltage')
					]);
			case 'CurrentSolve':
				return _List_fromArray(
					[
						$elm$html$Html$text('Voltage'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Resistance'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Current')
					]);
			default:
				return _List_fromArray(
					[
						$elm$html$Html$text('Voltage'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('/')
							])),
						$elm$html$Html$text('Current'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(altClass)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('=')
							])),
						$elm$html$Html$text('Resistance')
					]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$UI$pageHeader(formula),
				A2(
				$author$project$UI$cardsGrid,
				3,
				_List_fromArray(
					[
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Voltage'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve),
										'Solving for Voltage',
										'Solve for Voltage',
										$author$project$Pages$VoltageCurrentResistanceCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltsField, model.voltage, $author$project$Units$Metric$Base, 'Volts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$KilovoltsField, model.voltage, $author$project$Units$Metric$Kilo, 'Kilovolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$MegavoltsField, model.voltage, $author$project$Units$Metric$Mega, 'Megavolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$GigavoltsField, model.voltage, $author$project$Units$Metric$Giga, 'Gigavolts', $author$project$Units$Electricity$voltageToFloat, $author$project$Units$Electricity$formatVoltage, $author$project$Pages$VoltageCurrentResistanceCalculator$VoltageSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Current'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve),
										'Solving for Current',
										'Solve for Current',
										$author$project$Pages$VoltageCurrentResistanceCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$AmpsField, model.current, $author$project$Units$Metric$Base, 'Amps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$KiloampsField, model.current, $author$project$Units$Metric$Kilo, 'Kiloamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$MegaampsField, model.current, $author$project$Units$Metric$Mega, 'Megaamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$GigaampsField, model.current, $author$project$Units$Metric$Giga, 'Gigaamps', $author$project$Units$Electricity$currentToFloat, $author$project$Units$Electricity$formatCurrent, $author$project$Pages$VoltageCurrentResistanceCalculator$CurrentSolve)
									]))
							])),
						$author$project$UI$cardContainer(
						_List_fromArray(
							[
								A2(
								$author$project$UI$cardHeader,
								_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve),
								_List_fromArray(
									[
										$author$project$UI$cardTitle('Resistance'),
										A4(
										$author$project$UI$cardHeaderToggleButton,
										_Utils_eq(model.solveMethod, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve),
										'Solving for Resistance',
										'Solve for Resistance',
										$author$project$Pages$VoltageCurrentResistanceCalculator$SetSolveMethod($author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve))
									])),
								$author$project$UI$cardBody(
								_List_fromArray(
									[
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$OhmsField, model.resistance, $author$project$Units$Metric$Base, 'Ohms', $author$project$Units$Electricity$resistanceToFloat, $author$project$Units$Electricity$formatResistance, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$KiloohmsField, model.resistance, $author$project$Units$Metric$Kilo, 'Kiloohms', $author$project$Units$Electricity$resistanceToFloat, $author$project$Units$Electricity$formatResistance, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$MegaohmsField, model.resistance, $author$project$Units$Metric$Mega, 'Megaohms', $author$project$Units$Electricity$resistanceToFloat, $author$project$Units$Electricity$formatResistance, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve),
										A8($author$project$Pages$VoltageCurrentResistanceCalculator$renderField, model, $author$project$Pages$VoltageCurrentResistanceCalculator$GigaohmsField, model.resistance, $author$project$Units$Metric$Giga, 'Gigaohms', $author$project$Units$Electricity$resistanceToFloat, $author$project$Units$Electricity$formatResistance, $author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceSolve)
									]))
							]))
					])),
				A3(
				$author$project$UI$resourcesContainer,
				'Examples',
				'Click the questions below to auto-fill the form with the solution:',
				_List_fromArray(
					[
						A2(
						$author$project$UI$resourceLink,
						'What would be the voltage for a lightbulb using 4.16 amps of current with a resistance of 57.7 ohms?',
						$author$project$Pages$VoltageCurrentResistanceCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentResistanceCalculator$VoltageExample,
								$author$project$Units$Electricity$Ohms(57.7),
								$author$project$Units$Electricity$Amps(4.16)))),
						A2(
						$author$project$UI$resourceLink,
						'What would be the current for a flashlight bulb with a resistance of 4 ohms operating at 3 volts',
						$author$project$Pages$VoltageCurrentResistanceCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentResistanceCalculator$CurrentExample,
								$author$project$Units$Electricity$Ohms(4),
								$author$project$Units$Electricity$Volts(3)))),
						A2(
						$author$project$UI$resourceLink,
						'What would be the resistance of a lightbulb operating at 120 volts using 0.83 amps of current?',
						$author$project$Pages$VoltageCurrentResistanceCalculator$SetExample(
							A2(
								$author$project$Pages$VoltageCurrentResistanceCalculator$ResistanceExample,
								$author$project$Units$Electricity$Volts(120),
								$author$project$Units$Electricity$Amps(0.83))))
					]))
			]));
};
var $author$project$Main$view = function (model) {
	var currentView = function () {
		var _v0 = model.page;
		switch (_v0.$) {
			case 'NotFound':
				return $author$project$Main$notFoundView;
			case 'PowerTimeEnergyCalculator':
				var pageModel = _v0.a;
				return A2(
					$elm$html$Html$map,
					$author$project$Main$PowerTimeEnergyCalculatorMsg,
					$author$project$Pages$PowerTimeEnergyCalculator$view(pageModel));
			case 'VoltageCurrentPowerCalculator':
				var pageModel = _v0.a;
				return A2(
					$elm$html$Html$map,
					$author$project$Main$VoltageCurrentPowerCalculatorMsg,
					$author$project$Pages$VoltageCurrentPowerCalculator$view(pageModel));
			case 'VoltageCurrentResistanceCalculator':
				var pageModel = _v0.a;
				return A2(
					$elm$html$Html$map,
					$author$project$Main$VoltageCurrentResistanceCalculatorMsg,
					$author$project$Pages$VoltageCurrentResistanceCalculator$view(pageModel));
			case 'EnergyCostCalculator':
				var pageModel = _v0.a;
				return A2(
					$elm$html$Html$map,
					$author$project$Main$EnergyCostCalculatorMsg,
					$author$project$Pages$EnergyCostCalculator$view(pageModel));
			default:
				var pageModel = _v0.a;
				return A2(
					$elm$html$Html$map,
					$author$project$Main$FrequencyRevolutionsPolesCalculatorMsg,
					$author$project$Pages$FrequencyRpmPolesCalculator$view(pageModel));
		}
	}();
	return {
		body: _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('min-h-full')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$nav,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('bg-black border-b border-gray-200')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('mx-auto px-4 sm:px-6 lg:px-8')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('flex justify-between h-16')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('flex')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('flex-shrink-0 flex items-center')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$a,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$href('/electricity/power-time-energy')
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$img,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('lg:block h-6 w-auto'),
																				$elm$html$Html$Attributes$src('/images/calcoolators-logo.svg')
																			]),
																		_List_Nil)
																	]))
															])),
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('-my-px ml-10 flex space-x-8')
															]),
														_List_Nil)
													]))
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('p-8')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('bg-blue-100 py-2 px-4 mb-4 rounded-lg')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('CalcOOlators is in beta. Standby for more calculators for different topics. In the meantime, you can play around with a few energy-related calculators:'),
										A2(
										$elm$html$Html$ul,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$li,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$href('/electricity/power-time-energy'),
																$elm$html$Html$Attributes$class('link')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Power, Time, and Energy')
															]))
													])),
												A2(
												$elm$html$Html$li,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$href('/electricity/energy-cost'),
																$elm$html$Html$Attributes$class('link')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Energy Cost')
															]))
													])),
												A2(
												$elm$html$Html$li,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$href('/electricity/voltage-current-power'),
																$elm$html$Html$Attributes$class('link')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Voltage, Current, and Power')
															]))
													])),
												A2(
												$elm$html$Html$li,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$href('/electricity/voltage-current-resistance'),
																$elm$html$Html$Attributes$class('link')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Voltage, Current, and Resistance')
															]))
													])),
												A2(
												$elm$html$Html$li,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$href('/electricity/frequency-revolutions-poles'),
																$elm$html$Html$Attributes$class('link')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Frequency, RPMs, and Rotor Poles')
															]))
													]))
											]))
									])),
								currentView
							]))
					]))
			]),
		title: 'CalcOOlators'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{
		init: $author$project$Main$init,
		onUrlChange: $author$project$Main$UrlChanged,
		onUrlRequest: $author$project$Main$LinkClicked,
		subscriptions: function (_v0) {
			return $elm$core$Platform$Sub$none;
		},
		update: $author$project$Main$update,
		view: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (currentTime) {
			return $elm$json$Json$Decode$succeed(
				{currentTime: currentTime});
		},
		A2($elm$json$Json$Decode$field, 'currentTime', $elm$json$Json$Decode$int)))(0)}});}(this));