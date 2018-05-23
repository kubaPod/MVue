# MVue (still prototype)

Tools for creating Wolfram Cloud Objects with Vue.js based interfaces.

# Background

The idea is that `CloudDeploy[Vue[...]]` should generate a Vue.js based interface boundled with automatically generated Wolfram Cloud APIs.

It is NOT meant as a replacement for default `CloudDeploy` but rather a way to create more responsive / user friendly interface for simple use cases where default methods look overly sluggish since they need to support all 'types' and various typesetting features of Mathematica.

For very basic cases it should work out of the box while for more demanding it should at least set up a project which can be enhanced assuming a minimal familiarity with JavaScript.

:exclamation: `APIFunction` can drain your `$CloudCreditsAvailable` as fast as `CloudCDF` so be careful.

Currently interface is just a single html file with all custom css/scripts and with Vue/Vuetify/Axios libraries linked from outside, at the end it will support more modern modularized stup for folks familar with node/webpack etc.  
 
# What's there 
 
 ### Manipulate / CloudCDF
 
 Support for `Manipulate` is the first thing to do, it is still in progress. The idea is that controllers are handled client side and the body is refreshed via Cloud API. 
 
 Body response `"FormatFunction"` can be whichever from `$ExportFormats` but it is up to the user to choose carefully. E.g. `"HTML"` would be the best for textual/graid data, `"SVG"` is nice for `Graphics` but for output from mesh like functions (`CountourPlot` etc) it is probably better to use `"PNG"`.  
 
# TODO
