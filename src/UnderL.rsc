@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module UnderL

import Prelude;
import Types;
import Testing;
import ExactL;

KV str2kvUnderL(str x)
{
	KV r = ExactL::str2kvL(x);
	if (!isValidUnderL(r)) throw "parse error in <x>";
	return r;
}

str kv2strUnderL(KV x)
{
	if (!isValidUnderL(x)) throw "unparse error in <x>";
	return ExactL::kv2strL(x);
}

bool isValidUnderL(KV kv)
	= eqL(uniq(kv),kv) && isSorted([key | <key,val> <- kv]);

KV addPairUnderL(KV orig, str k, str v)
	= domainR(orig, {key | <key,val> <- orig, key <= k})
	+ <k,v>
	+ domainR(orig, {key | <key,val> <- orig, key > k})
	;
KV remPairUnderL(KV orig, str k, str v)
{
	if (!isValidUnderL(orig)) throw "remove error in <orig>";
	return orig - <k,v>;
}

test bool t_parseNone() = str2kvUnderL("{}") == [];
test bool t_parseEmpty() = str2kvUnderL("{\"\": \"\"}") == [<"","">];
test bool t_parseAB() = str2kvUnderL("{\"a\":\"b\"}") == [<"a","b">];
test bool t_parseABCD() = str2kvUnderL("{\"a\":\"b\", \"c\":\"d\"}") == [<"a","b">, <"c","d">];
test bool t_parseQuote() = str2kvUnderL("{\"\\\"\": \"\"}") == [<"\"","">];
test bool t_validQC(KV kv) = isValidUnderL(cleanup(kv));
test bool t_parseQC(str s1, str s2)
	= eqL(str2kvUnderL("{\"<escapeQ(cleanup(s1))>\":\"<escapeQ(cleanup(s2))>\"}"),[<cleanup(s1), cleanup(s2)>]);
test bool t_str2kv2str(str s1, str s2)
{
	str tc = "{\n\t\"<escapeQ(cleanup(s1))>\": \"<escapeQ(cleanup(s2))>\"\n}";
	return kv2strUnderL(str2kvUnderL(tc)) == tc;
}
test bool t_str2kv2str2kv(str s1, str s2)
{
	KV tc = [<cleanup(s1), cleanup(s2)>];
	return eqL(str2kvUnderL(kv2strUnderL(tc)), tc);
}
test bool t_add(KV tc_, str k, str v) = size(cleanup(tc_)) <= size(addPairUnderL(cleanup(tc_),k,v));
test bool t_rem(KV tc_, str k, str v) = !isValidUnderL(cleanup(tc_)) || size(cleanup(tc_)) >= size(remPairUnderL(cleanup(tc_),k,v));
