@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module Types

import List;

alias KV = lrel[str,str];

bool eqL(KV d1, KV d2) = isEmpty(d1 - d2) && isEmpty(d2 - d1);
