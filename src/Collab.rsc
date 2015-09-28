module Collab

import Types;
import ExactL;
import UnderL;
import OverL;
import Testing;

// Test case 1: add a tuple, remove a tuple
test bool addEremE(KV tc, str k, str v)
	= eqL(remPairL(addPairL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addEremU(KV tc, str k, str v)
	= eqL(remPairL(addPairUnderL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addEremO(KV tc, str k, str v)
	= eqL(remPairL(addPairOverL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addUremE(KV tc, str k, str v)
	= eqL(remPairUnderL(addPairL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addUremU(KV tc, str k, str v)
	= eqL(remPairUnderL(addPairUnderL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addUremO(KV tc, str k, str v)
	= eqL(remPairUnderL(addPairOverL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addOremE(KV tc, str k, str v)
	= eqL(remPairOverL(addPairL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addOremU(KV tc, str k, str v)
	= eqL(remPairOverL(addPairUnderL(cleanup(tc),k,v),k,v), cleanup(tc));
test bool addOremO(KV tc, str k, str v)
	= eqL(remPairOverL(addPairOverL(cleanup(tc),k,v),k,v), cleanup(tc));

// Test case 2: unparse, parse
test bool unparseEparseE(KV tc)
	= eqL(str2kvL(kv2strL(cleanup(tc))), cleanup(tc));
test bool unparseUparseE(KV tc)
	= eqL(str2kvL(kv2strUnderL(cleanup(tc))), cleanup(tc));
test bool unparseOparseE(KV tc)
	= eqL(str2kvL(kv2strOverL(cleanup(tc))), cleanup(tc));
test bool unparseEparseU(KV tc)
	= eqL(str2kvUnderL(kv2strL(cleanup(tc))), cleanup(tc));
test bool unparseUparseU(KV tc)
	= eqL(str2kvUnderL(kv2strUnderL(cleanup(tc))), cleanup(tc));
test bool unparseOparseU(KV tc)
	= eqL(str2kvUnderL(kv2strOverL(cleanup(tc))), cleanup(tc));
test bool unparseEparseO(KV tc)
	= eqL(str2kvOverL(kv2strL(cleanup(tc))), cleanup(tc));
test bool unparseUparseO(KV tc)
	= eqL(str2kvOverL(kv2strUnderL(cleanup(tc))), cleanup(tc));
test bool unparseOparseO(KV tc)
	= eqL(str2kvOverL(kv2strOverL(cleanup(tc))), cleanup(tc));
