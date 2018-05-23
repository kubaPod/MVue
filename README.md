# MVue (still prototype)

Tools for creating Wolfram Cloud Objects with Vue.js based interfaces.

# Background

The idea is that `CloudDeploy[Vue[...]]` should generate a Vue.js based interface boundled with automatically generated Wolfram Cloud APIs.

It is NOT meant as a replacement for default `CloudDeploy` but rather a way to create more responsive / user friendly interface for simple use cases where default methods look overly sluggish since they need to support all 'types' and various typesetting features of Mathematica.

For very basic cases it should work out of the box while for more demanding it should at least set up a project which can be enhanced assuming a minimal familiarity with JavaScript.

:exclamation: `APIFunction` can drain your `$CloudCreditsAvailable` as fast as `CloudCDF` so be careful.

Currently interface is just a single html file with all custom css/scripts and with Vue/Vuetify/Axios libraries linked from outside, at the end it will support more modern modularized stup for folks familar with node/webpack etc.  
 
# What's there 

 ### MVue
 
 `MVue` is a general constructor. Currently it only works with `Manipulate` and accepts the following options:
 
  - `"ExportFunction"` can be any of `$ExportFormats` or a function that returns a string that can be displayed by the browser, e.g.: `Function[body, ExportString[body, "HTMLFragment"]]`
  - `ContinuousAction` is `False` by default and unless is set to `True` it will overwrite default `Automatic` of `Manipulate's`. The reason is that it is better to call cloud only when controllers are released as opposed to doing this continuously. Because of `$CloudCredits` of course. But feel free to experiment with `Vue[ContinuousAction->True]`
 
 ### Manipulate / CloudCDF
 
 Support for `Manipulate` is the first thing to do, **it is still in progress**. The idea is that controllers are handled client side and the body is refreshed via Cloud API. 
 
 Body response `"FormatFunction"` can be whichever from `$ExportFormats` but it is up to the user to choose carefully. E.g. `"HTML"` would be the best for textual/graid data, `"SVG"` is nice for `Graphics` but for output from mesh like functions (`CountourPlot` etc) it is probably better to use `"PNG"`.  
 
 Here is one example that fails with default deployment:
 
 https://mathematica.stackexchange.com/q/172905/5478
 
     CloudDeploy[
      MVue["ExportFunction" -> "SVG"] @ Manipulate[
        LogLogPlot[
          Evaluate[ Table[
            (c (4.13*^19 a + 4.13*^19 c) + 10^b (1.36*^18 + c (1.36*^18 + 4.15*^16 a + 4.15*^16 c))
            )/(2.05*^17 a + 2.05*^17 c + 10^b (6.82*^15 + 2.06*^14 a + 2.06*^14 c))
          , {b, 1., 5, 1}]
          ]
        , {a, 10^-3, 10^3}
        , PlotLegends -> Table[10^b, {b, 1, 5, 1}]
        ]
      , {{c, 1/100}, 0, .1}
      ]
    ]
    
![Alt text](data/v-manipulate-simple.gif?raw=true "v-manipulate")    
 
# TODO

- v-manipulate
  - controllers 
    + slider for default manipulator :heavy_check_mark:
    + popup menu for default select :heavy_check_mark:
    + checkbox :heavy_check_mark:
    - labeled items (`val->lbl`) for select-like controllers
    - ControlType support  
  + api export function, png,html etc. :heavy_check_mark: (`"ExportFunction"`)       
  - SymbolName encoding/form?        
  - merge Vuetify props if provided        
  - dependency tree for body and dynamic structure at the end.
  - error handling for api function
 - general error handling review