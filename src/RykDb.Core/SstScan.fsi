namespace RykDb.Core

/// SSTable の 1エントリ。
type internal SstEntry =
    {
        Key: byte array
        SeqNo: uint64
        Value: SqlValue array option
    }

    member IsDelete: bool

module internal SstEntry =
    val make: byte array -> uint64 -> SqlValue array option -> SstEntry
    val compare: SstEntry -> SstEntry -> int
    val encodedValueLength: SstEntry -> int
    val writeValueTo: byte array -> int -> SstEntry -> int
    val decodeValueAt: Cursor -> int -> Result<uint64 * SqlValue array option * Cursor, DbError>
    val decodeValue: byte array -> Result<uint64 * SqlValue array option, DbError>
    val approximateBytes: SstEntry -> int

/// 1回のクエリで開いた走査の台帳。
type internal ScanScope =
    new: unit -> ScanScope
    member Register: key: obj * resource: System.IDisposable -> unit
    member Release: key: obj -> unit
    member DisposeAll: unit -> unit
    member internal ActiveCount: int

/// SSTable の遅延走査（one-shot）。
/// <remarks>
/// ケースはこのシグネチャで隠す。
/// 構築は `ofStep` / `ofArray` / `ofState` / `ofFilter` / `ofScoped` に限る。
/// </remarks>
[<Sealed>]
type internal SstScan =
    interface System.IDisposable

module internal SstScan =
    val ofStep: (unit -> Result<(SstEntry * SstScan) option, DbError>) -> SstScan

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
    val ofScoped: SstScan -> ScanScope -> obj -> SstScan
    val Empty: SstScan
    val next: (SstScan -> Result<(SstEntry * SstScan) option, DbError>)
    val dispose: SstScan -> unit
    val withScan: SstScan -> (SstScan -> Result<'a, DbError>) -> Result<'a, DbError>
    val scoped: ScanScope -> SstScan -> SstScan
    val emit: (unit -> SstScan) -> SstEntry -> SstScan
    val toArray: SstScan -> Result<SstEntry array, DbError>
    val filter: (SstEntry -> bool) -> SstScan -> SstScan

/// 開いたソースの k-way merge（ケースは非公開）。
type internal SstMerge

module internal SstMerge =
    val Empty: SstMerge
    val insert: int -> SstEntry -> SstScan -> SstMerge -> SstMerge
    val ofScans: SstScan array -> Result<SstMerge, DbError>
    val top: SstMerge -> SstEntry option
    val next: SstMerge -> Result<(SstEntry * SstMerge) option, DbError>
    val dispose: SstMerge -> unit
