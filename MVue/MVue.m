(* ::Package:: *)

(* ::Chapter::Closed:: *)
(*Meta*)


(* Author: Kuba Podkalicki *)
(* Notes:
     TODO:
       - option: content type png/html
       - option: continuous action   
   
       - dependency tree for body and dynamic structure at the end.
       
     short scheme:
       Manipulate[body, {var_i, __}..]    ----> VManipulate[<|"controllers" \[Rule] dataset_, "body" \[Rule] function_|>]
       
       CloudDeploy[VManipulate] --\[Rule] (body \[Rule] apifunction) + (v-manipulate-templ + controllers --\[Rule] app html)
*)



(* ::Chapter::Closed:: *)
(*Export*)


BeginPackage["MVue`"];


ClearAll["`*", "`*`*"]


MVue::usage = "MVue is a constructor for V* objects. V* objects are meant for special deployment procedure.";
VManipulate::usage = "Symbolic wrapper for specification object of a Vue.js based interface for Manipulate";

Begin["`Private`"];


(* ::Chapter:: *)
(*Content*)


(* ::Section:: *)
(*MVue*)


(*$supportedManipulateForm[body_, varSpec_]:= Verbatim[Manipulate][
    Pattern[body, Blank[]]
  , Pattern[varSpec, ({_Symbol, __}|{{_Symbol, __}, __} ) ..]
  , OptionsPattern[]
  ]*)


MVue::argpatt = "Currently MVue only recognizes Manipulate[body_, varSpec:({var_Symbol, __}|{{var_Symbol, __}, __} ) ..]";


MVue[
  m:Verbatim[Manipulate][
    body_
  , varSpec:({_Symbol, __}|{{_Symbol, __}, __} ) ..
  , OptionsPattern[]
  ]
]:=Module[
  { vars , temp}
, VManipulate @ ManipulateBlock[{varSpec},
    <|
      "controllers" -> VControl /@ {varSpec},
      "bodyFunction" -> ManipulateAPIFunction[body, varSpec]
    |>
  ]
];

MVue[opts:OptionsPattern]:=Function[expr, MVue[expr, opts]];  

MVue[___]:=(Message[MVue::argpatt]; $Failed)



(* ::Section:: *)
(*VManipulate*)


(* ::Section::Closed:: *)
(*VControl*)


VControl[{var_Symbol, min_?NumericQ, rest___}]:=VControl[{{var, min, SymbolName[var]}, min, rest}];


VControl[{{var_Symbol, inital_, label_String}, min_, max_, step_:0.01, rest___}
]:= <|
  "name" -> SymbolName[var]
, "label" -> label
, "type" -> "v-slider"
, "spec" -> <|"min" -> min, "max" -> max, "step" -> step|>  
|>


(* ::Section::Closed:: *)
(*ManipulateBlock*)


ManipulateBlock::usage = "ManipulateBlock[{varSpec}, expr] acts like Block[{x1, x2}, expr] "<>
  "where xi are extracted from varSpec which should follow a basic syntax for Manipulate's  controllers specification.";
  


ManipulateBlock // Attributes = HoldAll;


ManipulateBlock[{varSpec:({_Symbol, __}|{{_Symbol, __}, __} )..}]:=Module[
  {vars}
, vars = Hold[varSpec] /. { {{s_Symbol, __}, __}:> s, {s_Symbol, __}:>s}
; vars /. Hold[vars__]:> Function[expr, Block[{vars}, expr ], HoldAll]
];


ManipulateBlock[varSpec_, expr_]:=ManipulateBlock[varSpec][expr]


(* ::Section::Closed:: *)
(*ManipulateAPIFunction*)


ManipulateAPIFunction::usage = "Generates a function to be used in behind VManipulate's api function";


ManipulateAPIFunction // Attributes = HoldAll;


ManipulateAPIFunction[body_, varSpec__]:= With[
  { block = ManipulateBlock[{varSpec}] }
, Function @ block[  
    Module[
     { data = ImportString[FromCharacterCode @ HTTPRequestData[]["BodyBytes"],"RawJSON"]
     , result
     }
   ,  (*variables initialization*)
     KeyValueMap[
       Function[{key, value}, ToExpression[key, StandardForm, Function[sym,sym=value,HoldFirst]]]     
     , data
     ]
     
     (*body*)
   ; result = body
   
      (*formatting*) 
   ; ExportString[body, "HTMLFragment"]]
   ]
]


(* ::Chapter::Closed:: *)
(*End*)


End[];

EndPackage[];
