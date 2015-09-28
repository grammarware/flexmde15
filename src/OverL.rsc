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

KV addPairOverL(KV orig, str k, str v) = orig + <k,v>;
KV remPairOverL(KV orig, str k, str v) = orig - <k,v>;

test bool t_parseNone() = str2kvOverL("{}") == [];
test bool t_parseEmpty() = str2kvOverL("{\"\": \"\"}") == [<"","">];
test bool t_parseAB() = str2kvOverL("{\"a\":\"b\"}") == [<"a","b">];
test bool t_parseABCD() = str2kvOverL("{\"a\":\"b\", \"c\":\"d\"}") == [<"a","b">, <"c","d">];
test bool t_parseQuote() = str2kvOverL("{\"\\\"\": \"\"}") == [<"\"","">];
test bool t_validQC(KV kv) = isValidOverL(kv);
test bool t_parseQC(str s1, str s2)
	= str2kvOverL("{\"<escapeQ(cleanup(s1))>\":\"<escapeQ(cleanup(s2))>\"}")
	== [<cleanup(s1), cleanup(s2)>];
test bool t_str2kv2str(str s1, str s2)
{
	str tc = "{\n\t\"<escapeQ(cleanup(s1))>\": \"<escapeQ(cleanup(s2))>\"\n}";
	return kv2strOverL(str2kvOverL(tc)) == tc;
}
test bool t_str2kv2str2kv(str s1, str s2)
{
	KV tc = [<cleanup(s1), cleanup(s2)>];
	return str2kvOverL(kv2strOverL(tc)) == tc;
}
test bool t_add(KV tc_, str k, str v) = size(cleanup(tc_)) < size(addPairOverL(cleanup(tc_),k,v));
test bool t_rem(KV tc_, str k, str v) = size(cleanup(tc_)) >= size(remPairOverL(cleanup(tc_),k,v));
