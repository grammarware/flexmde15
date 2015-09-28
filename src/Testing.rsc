@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module Testing

import Types;
import Prelude;

str unescapeQ(str x) = replaceAll(x, "\\\"", "\"");
str escapeQ(str x) = replaceAll(x, "\"", "\\\"");

str cleanup(str x) = ( x | replaceAll(it, c , "_") | str c <- [" ", "\t", "\r", "\n", "\a00", "\u00a0"]);
KV cleanup(KV kv) = [<cleanup(k),cleanup(v)> | <k,v> <- kv];

bool isSorted(list[&T] xs) = sort(xs) == xs;
