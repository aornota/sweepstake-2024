module Aornota.Sweepstake2024.Ui.Common.LazyViewOrHMR

open Elmish.React.Common

(* Note: lazyView[n] functions do not play well with HMR - e.g. changes to render functions not apparent because state has not changed (so not re-rendered) - therefore suppress "laziness"
when HMR (see webpack.config.js) is defined. *)
let lazyViewOrHMR render state =
#if HMR
    render state
#else
    lazyView render state
#endif
let lazyViewOrHMR2 render state dispatch =
#if HMR
    render state dispatch
#else
    lazyView2 render state dispatch
#endif
