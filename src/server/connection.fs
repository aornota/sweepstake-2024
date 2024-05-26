module Aornota.Sweepstake2024.Server.Connection

open System

type ConnectionId = | ConnectionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> ConnectionId
