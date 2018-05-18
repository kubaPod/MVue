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


MVue::argpatt = "Currently MVue only recognizes Manipulate[body_, varSpec:({var_Symbol, __}|{{var_Symbol, __}, __} ) ..]";


MVue[
  m:Verbatim[Manipulate][
    body_
  , varSpec:({_Symbol, __}|{{_Symbol, __}, __} ) ..
  , OptionsPattern[]
  ]
]:=Module[
  { vars , temp}
, ManipulateBlock[{varSpec},
    <|
      "controllers" -> VControl /@ {varSpec},
      "body" -> {}
    |>
  ]
];

MVue[opts:OptionsPattern]:=Function[expr, MVue[expr, opts]];  

MVue[___]:=(Message[MVue::argpatt]; $Failed)



(* ::Section:: *)
(*ManipulateBlock*)


ManipulateBlock::usage = "ManipulateBlock[{varSpec}, expr] acts like Block[{x1, x2}, expr] "<>
  "where xi are extracted from varSpec which should follow a basic syntax for Manipulate's  controllers specification.";
  


ManipulateBlock // Attributes = HoldAll;


ManipulateBlock[{varSpec:({_Symbol, __}|{{_Symbol, __}, __} )..}]:=Module[
  {vars}
, vars = Hold[varSpec] /. { {{s_Symbol, __}, __}:> s, {s_Symbol, __}:>s}
; vars /. Hold[vars__]:> Function[expr, Block[{vars}, expr ]]
];


ManipulateBlock[varSpec_, expr_]:=ManipulateBlock[varSpec][expr]


(* ::Chapter::Closed:: *)
(*End*)


End[];

EndPackage[];
