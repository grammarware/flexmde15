@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module ExactL

import List; // intercalate
import Types;
import Testing;

layout WS = [\ \t\n\r]*;
start syntax L_JDN = "{" {KVPair ","}* kvs "}";
syntax KVPair = Str key ":" Str val;
lexical Str = [\"] Char* inner [\"];
lexical Char = ![\"] | [\\][\\\"] ;

KV str2kvL(str x)
{
	L_JDN pt = parse(#start[L_JDN], x).top;
	return
		(pt has kvs)
		? [<unescapeQ("<kv.key.inner>"),unescapeQ("<kv.val.inner>")> | KVPair kv <- pt.kvs]
		: [];
}

str kv2strL(KV x)
	= isEmpty(x)
	? "{\n}"
	: "{\n\t" + intercalate(",\n\t", ["\"<escapeQ(k)>\": \"<escapeQ(v)>\"" | <k,v> <- x]) + "\n}"
	;

bool isValidExactL(KV kv) = true; // we only need syntactic conformance

KV addPairL(KV orig, str k, str v) = orig + <k,v>;
KV remPairL(KV orig, str k, str v) = orig - <k,v>;

test bool t_parseNone() = str2kvL("{}") == [];
test bool t_parseEmpty() = str2kvL("{\"\": \"\"}") == [<"","">];
test bool t_parseAB() = str2kvL("{\"a\":\"b\"}") == [<"a","b">];
test bool t_parseABCD() = str2kvL("{\"a\":\"b\", \"c\":\"d\"}") == [<"a","b">, <"c","d">];
test bool t_parseQuote() = str2kvL("{\"\\\"\": \"\"}") == [<"\"","">];
test bool t_validQC(KV kv) = isValidExactL(kv);
test bool t_parseQC(str s1, str s2)
	= eqL(str2kvL("{\"<escapeQ(cleanup(s1))>\":\"<escapeQ(cleanup(s2))>\"}"), [<cleanup(s1), cleanup(s2)>]);
test bool t_str2kv2str(str s1, str s2)
{
	str tc = "{\n\t\"<escapeQ(cleanup(s1))>\": \"<escapeQ(cleanup(s2))>\"\n}";
	return kv2strL(str2kvL(tc)) == tc;
}
test bool t_str2kv2str2kv(str s1, str s2)
{
	KV tc = [<cleanup(s1), cleanup(s2)>];
	return eqL(str2kvL(kv2strL(tc)), tc);
}
test bool t_kv2str2kv(KV tc)
	= str2kvL(kv2strL(cleanup(tc))) == cleanup(tc);
test bool t_add(KV tc_, str k, str v) = size(cleanup(tc_)) < size(addPairL(cleanup(tc_),k,v));
test bool t_rem(KV tc_, str k, str v) = size(cleanup(tc_)) >= size(remPairL(cleanup(tc_),k,v));
test bool t_addrem(KV tc_, str k, str v)
{
	tc = cleanup(tc_);
	while (<k,v> in tc) k += "!"; // only needed for exact L
	return tc == remPairL(addPairL(tc,k,v),k,v);
}
