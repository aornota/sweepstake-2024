module Aornota.Sweepstake2024.Ui.Theme.Shared

open Aornota.Sweepstake2024.Ui.Theme.Light
open Aornota.Sweepstake2024.Ui.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeLight else themeDark
