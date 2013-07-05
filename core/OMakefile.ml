install Library ".DEFAULT" [
  (* Target *)
  Name		"corelib";
  Description	"Base library core";
  Version	"0.1";

  (* Sources *)
  Modules [
    "BaseOptions";
    "BasePervasives";
    (*"BitSet";*)
    "CharMap";
    "CharStream";
    "Cmdline";
    "CoreArray";
    "CoreFormat";
    "CoreInt";
    "CoreList";
    "CoreString";
    "HashStack";
    "IntMap";
    "IntegralModule";
    "Levenshtein";
    "Ptmap";
    (*"Sched";*)
    "SexpChar";
    "SexpHashtbl";
    "SexpMap";
    "SexpSet";
    "SexpString";
    "Sig";
    "SparseBitSet";
    "StringMap";
    "StringSet";
    "TermColour";
    (*"TestFramework";*)
    "Timing";
    (*"Valgrind";*)
    (*"Zlib";*)
  ];

  (* Library dependencies *)
  OCamlRequires [
    "deriving-ocsigen";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "integralModule.ml",	"-syntax camlp4o";
    "sig.ml",			"-syntax camlp4o";
  ];
];
