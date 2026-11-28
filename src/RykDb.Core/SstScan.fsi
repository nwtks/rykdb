namespace RykDb.Core

/// ブロック内の符号化された値の位置（削除タグ + 値本体）。
type internal RawSstValue

/// SSTable の 1エントリ。
type internal SstEntry =
    {
        Key: byte array
        SeqNo: SeqNo
        Value: SqlValue array option
        RawValue: RawSstValue option
    }

    member IsDelete: bool

module internal SstEntry =
    val make: byte array -> SeqNo -> SqlValue array option -> SstEntry
    val makeRaw: byte array -> SeqNo -> RawSstValue -> SstEntry
    val compare: SstEntry -> SstEntry -> int
    val encodedValueLength: SstEntry -> int
    val writeValueTo: byte array -> int -> SstEntry -> Result<int, DbError>
    val decodeValue: byte array -> Result<SeqNo * SqlValue array option, DbError>
    val decodeValueAt: int -> Cursor -> Result<SeqNo * SqlValue array option * Cursor, DbError>
    val decodeSkippedValueAt: int -> Cursor -> Result<SeqNo * SqlValue array option * Cursor, DbError>

    val decodeMaskedValueAt:
        bool array -> bool -> int -> Cursor -> Result<SeqNo * SqlValue array option * Cursor, DbError>

    val decodeRawValueAt: int -> Cursor -> Result<SeqNo * RawSstValue * Cursor, DbError>
    val decoded: SstEntry -> Result<SqlValue array option, DbError>
    val approximateBytes: SstEntry -> int

/// 1回のクエリで開いた走査の台帳。
type internal ScanScope =
    new: unit -> ScanScope
    member Register: key: obj * resource: System.IDisposable -> unit
    member Release: key: obj -> unit
    member Forget: key: obj -> unit
    member DisposeAll: unit -> unit
    member internal ActiveCount: int

/// SSTable の遅延走査（one-shot）。
[<Sealed>]
type internal SstScan =
    interface System.IDisposable

module internal SstScan =
    val Empty: SstScan
    val ofStep: (unit -> Result<(SstEntry * SstScan) option, DbError>) -> SstScan
    val ofStepWithClose: (unit -> Result<(SstEntry * SstScan) option, DbError>) -> (unit -> unit) -> SstScan

    val ofArray:
        SstEntry array ->
        int ->
        int ->
        int ->
        (int -> Result<(SstEntry * SstScan) option, DbError>) ->
        (unit -> unit) ->
            SstScan

    val ofState<'s> : 's -> ('s -> Result<(SstEntry * SstScan) option, DbError>) -> ('s -> unit) -> SstScan
    val ofFilter: (SstEntry -> bool) -> SstScan -> SstScan
    val ofScoped: obj -> ScanScope -> SstScan -> SstScan
    val next: SstScan -> Result<(SstEntry * SstScan) option, DbError>
    val dispose: SstScan -> unit
    val withResource: System.IDisposable -> SstScan -> SstScan
    val withScan: (SstScan -> Result<'a, DbError>) -> SstScan -> Result<'a, DbError>
    val scoped: ScanScope -> SstScan -> SstScan
    val emit: (unit -> SstScan) -> SstEntry -> SstScan
    val toArray: SstScan -> Result<SstEntry array, DbError>
    val filter: (SstEntry -> bool) -> SstScan -> SstScan

/// 開いたソースの k-way merge。
type internal SstMerge

module internal SstMerge =
    val Empty: SstMerge
    val insert: int -> SstEntry -> SstScan -> SstMerge -> SstMerge
    val ofScans: SstScan array -> Result<SstMerge, DbError>
    val top: SstMerge -> SstEntry option
    val next: SstMerge -> Result<(SstEntry * SstMerge) option, DbError>
    val dispose: SstMerge -> unit
