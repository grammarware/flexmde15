@contributor{Vadim Zaytsev - vadim@grammarware.net - UvA}
module Collab

import Types;
import ExactL;
import UnderL;
import OverL;
import Streamline;
import Testing;

bool addremNewPair(KV(KV,str,str) addfun, KV(KV) between, KV(KV,str,str) remfun, KV tc_, str k, str v)
{
	tc = cleanup(tc_);
	while (<k,v> in tc) k += "!";
	return eqL(remfun(between(addfun(tc,k,v)),k,v), tc);
}

bool parseunparsed(str(KV) theUnparser, str(str) between, KV(str) theParser, KV tc_)
{
	tc = cleanup(tc_);
	return eqL(theParser(between(theUnparser(tc))), tc);
}

// Test case 1: add a tuple, remove a tuple
test bool addEremE(KV tc, str k, str v)
	= addremNewPair(addPairL, id, remPairL, tc, k, v); 
test bool addEremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairL, tc, k, v); 
test bool addEremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, id, remPairL, tc, k, v); 
test bool addUremE(KV tc, str k, str v)
	= addremNewPair(addPairL, id, remPairUnderL, tc, k, v); 
test bool addUremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairUnderL, tc, k, v); 
test bool addUremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, id, remPairUnderL, tc, k, v); 
test bool addOremE(KV tc, str k, str v)
	= addremNewPair(addPairL, id, remPairOverL, tc, k, v);
test bool addOremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairOverL, tc, k, v);
test bool addOremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, id, remPairOverL, tc, k, v); 

// Test case 2: unparse, parse
test bool unparseEparseE(KV tc)
	= parseunparsed(kv2strL, idS, str2kvL, tc); 
test bool unparseUparseE(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvL, tc);
test bool unparseOparseE(KV tc)
	= parseunparsed(kv2strOverL, idS, str2kvL, tc);
test bool unparseEparseU(KV tc)
	= parseunparsed(kv2strL, idS, str2kvUnderL, tc); 
test bool unparseUparseU(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvUnderL, tc); 
test bool unparseOparseU(KV tc)
	= parseunparsed(kv2strOverL, idS, str2kvUnderL, tc); 
test bool unparseEparseO(KV tc)
	= parseunparsed(kv2strL, idS, str2kvOverL, tc);
test bool unparseUparseO(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvOverL, tc);
test bool unparseOparseO(KV tc)
	= parseunparsed(kv2strOverL, idS, str2kvOverL, tc);

//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////// STREAMLINERS ////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

// Test case 1a: add a tuple, remove a tuple
test bool s_addEremE(KV tc, str k, str v)
	= addremNewPair(addPairL, id, remPairL, tc, k, v); 
test bool s_addEremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairL, tc, k, v); 
test bool s_addEremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, id, remPairL, tc, k, v); 
test bool s_addUremE(KV tc, str k, str v)
	= addremNewPair(addPairL, canonise, remPairUnderL, tc, k, v); // canoniser 
test bool s_addUremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairUnderL, canonise(cleanup(tc)), k, v); // work on canonic data
test bool s_addUremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, normalise, remPairUnderL, tc, k, v); // normaliser
test bool s_addOremE(KV tc, str k, str v)
	= addremNewPair(addPairL, id, remPairOverL, tc, k, v);
test bool s_addOremU(KV tc, str k, str v)
	= addremNewPair(addPairUnderL, id, remPairOverL, tc, k, v);
test bool s_addOremO(KV tc, str k, str v)
	= addremNewPair(addPairOverL, id, remPairOverL, tc, k, v); 

// Test case 2a: unparse, parse
test bool s_unparseEparseE(KV tc)
	= parseunparsed(kv2strL, idS, str2kvL, tc); 
test bool s_unparseUparseE(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvL, canonise(cleanup(tc))); // work on canonic data
test bool s_unparseOparseE(KV tc)
	= parseunparsed(kv2strOverL, codifyS, str2kvL, tc); // codifier
test bool s_unparseEparseU(KV tc)
	= parseunparsed(kv2strL, canoniseS, str2kvUnderL, tc); // canoniser 
test bool s_unparseUparseU(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvUnderL, canonise(cleanup(tc))); // work on canonic data
test bool s_unparseOparseU(KV tc)
	= parseunparsed(kv2strOverL, normaliseS, str2kvUnderL, tc); // normaliser 
test bool s_unparseEparseO(KV tc)
	= parseunparsed(kv2strL, idS, str2kvOverL, tc);
test bool s_unparseUparseO(KV tc)
	= parseunparsed(kv2strUnderL, idS, str2kvOverL, canonise(cleanup(tc))); // work on canonic data
test bool s_unparseOparseO(KV tc)
	= parseunparsed(kv2strOverL, idS, str2kvOverL, tc);
