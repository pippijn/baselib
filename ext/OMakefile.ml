install Library ".DEFAULT" [
  (* Target *)
  Name		"baselib";
  Description	"Base library";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Bit2d";
    "BitSet";
    "DenseIntMap";
    "ExtString";
    "Sched";
    "TestFramework";
    "Valgrind";
    "Zlib";
  ];

  Sources [
    "ml_BitSet.cpp";
    "ml_Sched.c";
    "ml_Valgrind.cpp";
    "ml_Zlib.c";
  ];

  Headers [
    "ml_BitSet-custom.h";
    "ml_BitSet-raw.h";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "batteries";
    "corelib";
    "sexplib.syntax";
  ];

  (* Link with zlib *)
  CRequires [
    "z";
  ];

  (* Camlp4 *)
  Flags [
    "bit2d.ml",		"-syntax camlp4o";
    "denseIntMap.ml",	"-syntax camlp4o";
  ];
]
