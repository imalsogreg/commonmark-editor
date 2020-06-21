A reflex app using commonmark-hs to render markdown into html

## Building

*ghc*
``` sh
path/to/reflex-platform/scripts/work-on ./ghc-env.nix ./.
ghcid --command "cabal repl" -T "Commonmark.App.debug"
```

*ghcjs*

``` sh
path/to/reflex-platform/scripts/work-on ./ghcjs-env.nix ./.
cabal build --ghcjs
```

