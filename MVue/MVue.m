(* ::Package:: *)

(* ::Chapter:: *)
(*Meta*)


(* Author: Kuba Podkalicki *)

(* The concept (based on Manipulate case)

    MVue @ Manipulate should produce a json/js friendly object with controllers specification + a body function for API
    json is feed into Vue.js based interface.
    interface is deployed to {cloud}/{path}/app and API to ./bodyAPI
 *)
 


(* 
     TODO:
       
       - support for basic controllers
         + slider
         + popup menu
         + checkbox
         - labeled items for select-like controllers
         - via ControlType
           - input field
           - toggler
           - setter         
         
       - error handling
       - option: content type png/html       
       - SymbolName encoding/form?
       
       - merge vuetify props if provided      
   
       - dependency tree for body and dynamic structure at the end.
       - error handling for api function
*)


(* ::Chapter:: *)
(*Export*)


BeginPackage["MVue`"];


ClearAll["`*", "`*`*"]


MVue::usage = "MVue is a constructor for V* objects. V* objects are meant for special deployment procedure.";
VManipulate::usage = "Symbolic wrapper for specification object of a Vue.js based interface for Manipulate";

Begin["`Private`"];


(* ::Chapter:: *)
(*Content*)


$resources = FileNameJoin[{DirectoryName[$InputFileName /. "" :> NotebookFileName[]], "Resources"}];


$jsonAtoms = True | False | Null | _String | _?NumberQ


(* ::Section:: *)
(*MVue*)


(*$supportedManipulateForm[body_, varSpec_]:= Verbatim[Manipulate][
    Pattern[body, Blank[]]
  , Pattern[varSpec, ({_Symbol, __}|{{_Symbol, __}, __} ) ..]
  , OptionsPattern[]
  ]*)


MVue::argpatt = "Currently MVue only recognizes Manipulate[body_, varSpec:({var_Symbol, __}|{{var_Symbol, __}, __} ) ..]";


MVue[m_Manipulate, opts___?OptionQ]:= VManipulate[m, opts]


MVue[opts___?OptionQ]:=Function[expr, MVue[expr, opts]];  


MVue[___]:=(Message[MVue::argpatt]; $Failed)


(* ::Section:: *)
(*VManipulate*)


VManipulate // Options = {
  ContinuousAction -> False  
};  


VManipulate[
  m : Verbatim[Manipulate][
    body    : _
  , varSpec : ({_Symbol, __}|{{_Symbol, __}, __} ) ..
  , opts    : OptionsPattern[]
  ]
, vopts : ___?OptionQ
]:=Module[  {  config}

, config = KeyMap[ Decapitalize @* ToString ] @ <|    
    Options[VManipulate]
  , FilterRules[{vopts}, Options[VManipulate]]   
  , FilterRules[{opts}, Options[VManipulate]]  
  |>
  
; VManipulate @ ManipulateBlock[{varSpec},
    <|"controllers" -> VControl /@ {varSpec}
    , "bodyFunction" -> ManipulateAPIFunction[body, varSpec, Evaluate[Sequence @@ FilterRules[{vopts}, Options @ ManipulateAPIFunction]]]
    , config
    |>
  ]
];


VManipulate /: CloudDeploy[vm_VManipulate, HoldPattern[p_String : CreateUUID[]], rest:OptionsPattern[]]:= Catch @ Module[
  {app, api, path = p}
, app = Import[ FileNameJoin[{$resources, "v-manipulate-simple-template.html"}]  , "Text"]
; app = StringTemplate[  app ] @ StringJoin[
    "`"
  , Check[ExportString[  KeyDrop["bodyFunction"] @ First @ vm, "RawJSON", "Compact"->True], Throw[$Failed]]
  , "`"
  ] 
; app = CloudExport[app, "HTML", path <> "/index.html", rest]   

; api = APIFunction[{}, Evaluate @ vm[[1, "bodyFunction"]]]
; api = CloudDeploy[api, path<>"/bodyAPI", rest]

; app
]


(* ::Section::Closed:: *)
(*VControl*)


(*It does not need HoldAll because it is run in ManipulateBlock anyway, 
  this way overloading is more flexible*)


(* ::Subsection:: *)
(*common*)


VControl[{{var_Symbol, init_}, rest___}]:=VControl[{{var, init, SymbolName[var]}, rest}]


(* ::Subsection::Closed:: *)
(*sliders*)


VControl[{var_Symbol, min_?NumericQ, rest___}]:=VControl[{{var, min}, min, rest}];


VControl[
  { {var_Symbol, initial_?NumericQ, label_String}
  , min_?NumericQ
  , max_?NumericQ
  , step : _?NumericQ : 0.01
  , rest___
  }
]:= <|
  "name" -> SymbolName[var]
, "label" -> label
, "init" -> initial
, "type" -> "v-slider"
, "spec" -> <|"min" -> min, "max" -> max, "step" -> step|>  
|>


(* ::Subsection:: *)
(*selects*)


VControl[{var_Symbol,items: { $jsonAtoms... }, rest___}]:=VControl[{{var, items[[1]]}, items,rest}]


VControl[{{var_Symbol, init_, lbl: $jsonAtoms}, items: {$jsonAtoms...},rest___}]:= <|
  "name" -> SymbolName[var]
, "lable"->lbl
, "init" -> init
, "type" -> "v-select"
, "spec" -> <|"items"->items|>    
|>


(* ::Subsection:: *)
(*checkboxes*)


VControl[{{var_Symbol, init_, lbl_String},items:{True,False},rest___}]:= <|
  "name" -> SymbolName[var]
, "lable"->lbl
, "init" -> init
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
; vars = DeleteDuplicates @ vars
; vars /. Hold[vars__]:> Function[expr, Block[{vars}, expr ], HoldAll]
];


ManipulateBlock[varSpec_, expr_]:=ManipulateBlock[varSpec][expr]


(* ::Section:: *)
(*ManipulateAPIFunction*)


ManipulateAPIFunction::usage = "Generates a function to be used in behind VManipulate's api function";


ManipulateAPIFunction // Attributes = HoldAll;


ManipulateAPIFunction // Options = {
  "ExportFunction" -> Automatic  
};


ManipulateAPIFunction[body_, varSpec__List, opts:OptionsPattern[]]:= With[
  { block = ManipulateBlock[{varSpec}]
  , exportFunction = resolveExportFunction @ OptionValue @ "ExportFunction" 
  }
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
   ; exportFunction @ body
   ]
 ]
]


resolveExportFunction[ s_String /; MemberQ[$ExportFormats, s] ]:=Function[b, ExportString[b, s]]
resolveExportFunction[foo: (_Symbol | _Function)]:=foo;
resolveExportFunction[(*Automatic or incorrect*)]:=Function[b, ExportString[b, "HTMLFragment"]]
(*TODO: more verbose handling of an invalid value*)


(* ::Chapter::Closed:: *)
(*End*)


End[];

EndPackage[];
