install Library ".DEFAULT" [
  (* Target *)
  Name		"corelib";
  Description	"Base library core";

  (* Sources *)
  Modules [
    "CharMap";
    "CharStream";
    "Cmdline";
    "CoreArray";
    "CoreFormat";
    "CoreInt";
    "CoreList";
    "CoreOption";
    "CoreOptions";
    "CorePervasives";
    "CoreString";
    "HashStack";
    "IntMap";
    "IntegralModule";
    "Levenshtein";
    "Ptmap";
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
    "Timing";
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
