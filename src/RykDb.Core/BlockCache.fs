// ===========================================================================
// BlockCache — (fileId, blockId) -> byte[] の LRU（不変実装）。
// 順序は「新しい順」のリスト、要素数は Map で数える。
// 上限はブロック数で数える（バイト数だと全ブロックの走査が必要になるため）。
// SSTable は圧縮しないので、読んだブロックをそのままメモリに載せる。
// ===========================================================================

namespace RykDb.Core

/// 1 つのキャッシュエントリ。
type internal CacheEntry =
    {
        FileNumber: int64
        BlockId: int
        Data: byte array
    }

/// LRU キャッシュの不変状態。
type internal BlockCache =
    {
        /// 上限（ブロック数）
        Capacity: int
        /// 新しい順
        Entries: CacheEntry list
    }

module internal BlockCache =
    let create (capacity: int) =
        {
            Capacity = max 0 capacity
            Entries = []
        }

    let private entryKey (entry: CacheEntry) = entry.FileNumber, entry.BlockId

    /// キャッシュを読む（あれば最近使った順に更新して返す）。
    let get (cache: BlockCache) (fileNumber: int64) (blockId: int) =
        let k = fileNumber, blockId

        match cache.Entries |> List.tryFind (fun e -> entryKey e = k) with
        | None -> None
        | Some found ->
            let rest = cache.Entries |> List.filter (fun e -> entryKey e <> k)
            let updated = { cache with Entries = found :: rest }
            Some(updated, found.Data)

    /// キャッシュへ入れる（既存は置き換えて最近使ったものにする）。
    let put (cache: BlockCache) (entry: CacheEntry) =
        let k = entryKey entry
        let rest = cache.Entries |> List.filter (fun e -> entryKey e <> k)

        let trimmed =
            if cache.Capacity <= 0 then
                []
            else
                entry :: rest |> List.truncate cache.Capacity

        { cache with Entries = trimmed }

    /// ファイルごと invalid する（コンパクションでファイルが消えたとき）。
    let invalidateFile (cache: BlockCache) (fileNumber: int64) =
        { cache with
            Entries = cache.Entries |> List.filter (fun e -> e.FileNumber <> fileNumber)
        }
