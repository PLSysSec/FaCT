open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;
ocaml_lib ~extern:true "llvm_analysis";;
ocaml_lib ~extern:true "llvm_target";;
ocaml_lib ~extern:true "llvm_bitwriter";;
ocaml_lib ~extern:true "llvm_bitreader";;
ocaml_lib ~extern:true "llvm_X86";;

flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"]);;
