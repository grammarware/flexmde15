@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module OverL

import Prelude;
import ListRelation;
import Types;
import Testing;

layout WS = [\ \t\n\r]*;
start syntax OverL_JDN = "{" {KVPair ","}* kvs "}";
syntax KVPair = {Str "&"}+ keys ":" Str val;
lexical Str = [\"] Char* inner [\"];
lexical Char = ![\"] | [\\][\\\"] ;

KV str2kvOverL(str x)
{
	OverL_JDN pt = parse(#start[OverL_JDN], x).top;
	return
		(pt has kvs)
		? [<unescapeQ("<k.inner>"),unescapeQ("<kv.val.inner>")> | KVPair kv <- pt.kvs, Str k <- kv.keys]
		: [];
}

str kv2strOverL(KV x)
{
	if (isEmpty(x)) return "{\n}";
	m = ListRelation::index(invert(x));
	return "{\n\t"
	+ intercalate(",\n\t",
		[ "\"<intercalate("\" & \"", [escapeQ(v) | v <- m[k]])>\": \"<escapeQ(k)>\"" | k <- m]) + "\n}"
	;
}

bool isValidOverL(KV kv) = true; // syntactic conformance is enough

// TODO?
KV addPairOverL(KV orig, str k, str v) = orig + <k,v>;
KV remPairOverL(KV orig, str k, str v) = orig - <k,v>;

test bool parseNone() = str2kvOverL("{}") == [];
test bool parseEmpty() = str2kvOverL("{\"\": \"\"}") == [<"","">];
test bool parseAB() = str2kvOverL("{\"a\":\"b\"}") == [<"a","b">];
test bool parseABCD() = str2kvOverL("{\"a\":\"b\", \"c\":\"d\"}") == [<"a","b">, <"c","d">];
test bool parseQuote() = str2kvOverL("{\"\\\"\": \"\"}") == [<"\"","">];
test bool validQC(KV kv) = isValidOverL(kv);
test bool parseQC(str s1, str s2)
	= str2kvOverL("{\"<escapeQ(cleanup(s1))>\":\"<escapeQ(cleanup(s2))>\"}")
	== [<cleanup(s1), cleanup(s2)>];
test bool str2kv2str(str s1, str s2)
{
	str tc = "{\n\t\"<escapeQ(cleanup(s1))>\": \"<escapeQ(cleanup(s2))>\"\n}";
	return kv2strOverL(str2kvOverL(tc)) == tc;
}
test bool str2kv2str2kv(str s1, str s2)
{
	KV tc = [<cleanup(s1), cleanup(s2)>];
	return str2kvOverL(kv2strOverL(tc)) == tc;
}
test bool kv2str2kv(KV tc)
	= eqL(str2kvOverL(kv2strOverL(cleanup(tc))), cleanup(tc));
test bool addrem(KV tc_, str k, str v)
{
	tc = cleanup(tc_);
	while (<k,v> in tc) k += "!"; // only needed for exact L
	return tc == remPairOverL(addPairOverL(tc,k,v),k,v);
}
