(* ::Package:: *)

(* ::Chapter:: *)
(*Meta*)


(* Author: Kuba Podkalicki *)

(* The concept (based on Manipulate case)

    MVue @ Manipulate should produce a json/js friendly object with controllers specification + a body function for API
    json is feed into Vue.js based interface.
    interface is deployed to {cloud}/{path}/app and API to ./bodyAPI
       

(* 
     TODO:
       - support for basic controllers
       - error handling
       - option: content type png/html
       - option: continuous action   
       - handle initial values
       - SymbolName encoding/form?
       - ControlType support
       - merge vuetify props if provided
       
   
       - dependency tree for body and dynamic structure at the end.
       - error handling for api function
       
       
       
       
       
       
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


$resources = FileNameJoin[{DirectoryName[$InputFileName /. "" :> NotebookFileName[]], "Resources"}];


(* ::Section::Closed:: *)
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


VManipulate /: CloudDeploy[vm_VManipulate, path_String, rest___]:= Module[
  {app, api}
, app = Import[ FileNameJoin[{$resources, "v-manipulate-simple-template.html"}]  , "Text"]
; app = StringTemplate[  app ] @ (
    "`" <> ExportString[  vm[[1,"controllers"]], "RawJSON", "Compact"->True] <> "`"
  )
; app = CloudExport[app, "HTML", path <> "/app", rest]   

; api = APIFunction[{}, Evaluate @ vm[[1, "bodyFunction"]]]
; api = CloudDeploy[api, path<>"/bodyAPI", rest]

; app
]


(* ::Section:: *)
(*VControl*)


(*It does not need HoldAll because it is run in ManipulateBlock anyway, 
  this way overloading is more flexible*)


(* ::Subsection:: *)
(*common*)


VControl[{{var_Symbol, init_}, rest___}]:=VControl[{{var, init, SymbolName[var]}, rest}]


(* ::Subsection:: *)
(*sliders*)


VControl[{var_Symbol, min_?NumericQ, rest___}]:=VControl[{{var, min}, min, rest}];


VControl[
  { {var_Symbol, inital_?NumericQ, label_String}
  , min_?NumericQ
  , max_?NumericQ
  , step : _?NumericQ : 0.01
  , rest___
  }
]:= <|
  "name" -> SymbolName[var]
, "label" -> label
, "type" -> "v-slider"
, "spec" -> <|"min" -> min, "max" -> max, "step" -> step|>  
|>


(* ::Subsection::Closed:: *)
(*selects*)


VControl[{var_Symbol,items_List,rest___}]:=VControl[{{var, items[[1]]}, items,rest}]


VControl[{{var_Symbol, init_, lbl_String},items_List,rest___}]:= <|
  "name" -> SymbolName[var]
, "lable"->lbl
, "type" -> "v-select"
, "spec" -> <|"items"->items|>    
|>


(* ::Subsection:: *)
(*checkboxes*)


VControl[{{var_Symbol, init_, lbl_String},items:{True,False},rest___}]:= <|
  "name" -> SymbolName[var]
, "lable"->lbl
, "type" -> "v-checkbox"
|>


(* ::Subsection:: *)
(*argx*)


VControl[___]:=$Failed;


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
