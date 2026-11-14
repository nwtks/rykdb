namespace RykDb.Core

/// memtable の 1エントリ。
type internal Entry =
    {
        /// 論理 keyspace の接頭辞を含む物理キー
        Key: byte array
        /// 書き込みの論理時計。大きいほど新しい
        SeqNo: uint64
        /// Some が値、None が削除 tombstone
        Value: SqlValue array option
    }

    member IsDelete: bool

module internal Entry =
    val put: byte array -> uint64 -> SqlValue array -> Entry
    val delete: byte array -> uint64 -> Entry
    val managedBytes: Entry -> int
    val maxSeqNo: seq<Entry> -> uint64
    val compareKeys: Entry -> Entry -> int
    val priority: Entry -> uint64
    val takeOwnership: Entry -> Entry
    val encodeBatch: TxnId -> Entry array -> byte array
    val decodeBatch: byte array -> Result<TxnId * Entry array, DbError>

/// memtable（永続 Treap）。
/// <remarks>
/// ケースはこのシグネチャで隠す。
/// 構築は `Memtable.Empty` / `insert` / `apply` に限る。
/// </remarks>
type internal Memtable

module internal Memtable =
    val Empty: Memtable
    val insert: Entry -> Memtable -> Memtable
    val apply: seq<Entry> -> Memtable -> Memtable
    val getVisible: byte array -> uint64 option -> Memtable -> Entry option
    val rangeScan: byte array -> int -> Memtable -> seq<Entry>
    val isEmpty: Memtable -> bool
    val approxBytes: Memtable -> int
