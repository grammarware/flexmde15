@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module Streamline

import Types;
import ExactL;
import UnderL;
import OverL;
import Testing;
import List;

// from exact-L to exact-L
KV id(KV kv) = kv;
str idS(str d) = d;

// from exact-L to under-L
KV canonise(KV kv) = sort(kv);
str canoniseS(str d) = kv2strUnderL(canonise(str2kvL(d)));

// from over-L to exact-L
KV codify(KV kv) = kv;
str codifyS(str d) = kv2strL(str2kvOverL(d));

// from over-L to under-L
KV normalise(KV kv) = sort(kv);
str normaliseS(str d) = kv2strUnderL(normalise(str2kvOverL(d)));

test bool t_canon(KV tc) = isValidUnderL(canonise(cleanup(tc)));
test bool t_code(KV tc) = isValidExactL(codify(cleanup(tc)));
test bool t_norm(KV tc) = isValidUnderL(normalise(cleanup(tc)));
