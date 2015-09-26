@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module ExactL

import Prelude;
import Types;

layout WS = [\ \t\n\r]*;
start syntax L_JDN = "{" {KVPair ","}* kvs "}";
syntax KVPair = Str key ":" Str val;
lexical Str = [\"] Char* inner [\"];
lexical Char = ![\"] | [\\][\\\"] ;

KV str2kvL(str x)
{
	L_JDN pt = parse(#start[L_JDN], x).top;
	if (pt has kvs)
		return [<unescapeQ("<kv.key.inner>"),unescapeQ("<kv.val.inner>")> | KVPair kv <- pt.kvs];
	else
		return [];
}
str kv2strL(KV x)
	= isEmpty(x)
	? "{\n}"
	: "{\n\t" + intercalate(",\n\t", ["\"<escapeQ(k)>\": \"<escapeQ(v)>\"" | <k,v> <- x]) + "\n}"
	;

KV addPairL(KV orig, str k, str v) = orig + <k,v>;
KV remPairL(KV orig, str k, str v) = orig - <k,v>;

private str unescapeQ(str x) = replaceAll(x, "\\\"", "\"");
private str escapeQ(str x) = replaceAll(x, "\"", "\\\"");

test bool parseNone() = str2kvL("{}") == [];
test bool parseEmpty() = str2kvL("{\"\": \"\"}") == [<"","">];
test bool parseAB() = str2kvL("{\"a\":\"b\"}") == [<"a","b">];
test bool parseABCD() = str2kvL("{\"a\":\"b\", \"c\":\"d\"}") == [<"a","b">, <"c","d">];
test bool parseQuote() = str2kvL("{\"\\\"\": \"\"}") == [<"\"","">];
test bool parseQCParse(str s1, str s2)
	= str2kvL("{\"<escapeQ(cleanup(s1))>\":\"<escapeQ(cleanup(s2))>\"}")
	== [<cleanup(s1), cleanup(s2)>];
test bool parseQCStr2KV2Str(str s1, str s2)
{
	str tc = "{\n\t\"<escapeQ(cleanup(s1))>\": \"<escapeQ(cleanup(s2))>\"\n}";
	return kv2strL(str2kvL(tc)) == tc;
}
test bool parseQCStr2KV2Str2KV(str s1, str s2)
{
	KV tc = [<cleanup(s1), cleanup(s2)>];
	return str2kvL(kv2strL(tc)) == tc;
}
test bool parseQCKV2Str2KV(KV tc) = str2kvL(kv2strL(cleanup(tc))) == cleanup(tc);
test bool parseQCAddRem(KV tc_, str k, str v)
{
	tc = cleanup(tc_);
	while (<k,v> in tc) k += "!"; // only needed for exact L
	return tc == remPairL(addPairL(tc,k,v),k,v);
}

private str cleanup(str x) = ( x | replaceAll(it, c , "_") | str c <- [" ", "\t", "\r", "\n", "\a00", "\u00a0"]);
private KV cleanup(KV kv) = [<cleanup(k),cleanup(v)> | <k,v> <- kv];
