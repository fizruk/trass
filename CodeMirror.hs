module CodeMirror where

import Import
import Control.Arrow
import Yesod.Core (Route)

data CMMode
  = CM_APL
  | CM_Asterisk
  | CM_CLike
  | CM_Clojure
  | CM_Cobol
  | CM_CoffeeScript
  | CM_CommonLisp
  | CM_CSS
  | CM_D
  | CM_Diff
  | CM_DTD
  | CM_Eiffel
  | CM_Erlang
  | CM_Fortran
  | CM_Gas
  | CM_Gherkin
  | CM_Go
  | CM_Groovy
  | CM_Haml
  | CM_Haskell
  | CM_Haxe
  | CM_HtmlEmbedded
  | CM_HtmlMixed
  | CM_HTTP
  | CM_Jade
  | CM_Javascript
  | CM_Jinja2
  | CM_Julia
  | CM_Less
  | CM_LiveScript
  | CM_Lua
  | CM_Markdown
  | CM_Mirc
  | CM_Nginx
  | CM_Ntriples
  | CM_OCaml
  | CM_Octave
  | CM_Pascal
  | CM_PegJs
  | CM_Perl
  | CM_PHP
  | CM_Pig
  | CM_Properties
  | CM_Python
  | CM_Q
  | CM_R
  | CM_RPM
  | CM_Rst
  | CM_Ruby
  | CM_Rust
  | CM_Sass
  | CM_Scheme
  | CM_Shell
  | CM_Sieve
  | CM_Smalltalk
  | CM_Smarty
  | CM_SmartyMixied
  | CM_Sparql
  | CM_SQL
  | CM_Stex
  | CM_Tcl
  | CM_TiddlyWiki
  | CM_Tikky
  | CM_Toml
  | CM_Turtle
  | CM_VB
  | CM_VBScript
  | CM_Velocity
  | CM_Verilog
  | CM_XML
  | CM_XQuery
  | CM_Yaml
  | CM_Z80
  deriving (Eq, Show)

showMode :: CMMode -> String
showMode = drop 3 . show

codeMirrorModes :: [(CMMode, Route App)]
codeMirrorModes = map (second StaticR) $
  [ (CM_Haskell, codemirror_mode_haskell_haskell_js)
  , (CM_Erlang,  codemirror_mode_erlang_erlang_js)
  ]

