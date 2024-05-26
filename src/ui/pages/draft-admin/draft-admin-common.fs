module Aornota.Sweepstake2024.Ui.Pages.DraftAdmin.Common

open Aornota.Sweepstake2024.Common.Domain.Draft
open Aornota.Sweepstake2024.Common.Revision
open Aornota.Sweepstake2024.Common.WsApi.ServerMsg
open Aornota.Sweepstake2024.Common.WsApi.UiMsg
open Aornota.Sweepstake2024.Ui.Common.Notifications
open Aornota.Sweepstake2024.Ui.Shared

open System.Collections.Generic

type ProcessDraftInput =
    | ConfirmProcessDraft
    | CancelProcessDraft

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerDraftAdminMsg of serverDraftAdminMsg : ServerDraftAdminMsg
    | ShowProcessDraftModal of draftId : DraftId
    | ProcessDraftInput of processDraftInput : ProcessDraftInput

type UserDraftSummaryDic = Dictionary<UserDraftKey, UserDraftSummaryDto>

type ProcessDraftStatus =
    | ProcessDraftPending
    | ProcessDraftFailed of errorText : string

type ProcessDraftState = {
    DraftId : DraftId
    ProcessDraftStatus : ProcessDraftStatus option }

type State = {
    UserDraftSummaryProjection : Projection<Rvn * UserDraftSummaryDic>
    ProcessDraftState : ProcessDraftState option }
