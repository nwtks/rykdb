// ===========================================================================
// BlockCache.fs（LRU ブロックキャッシュ）
// ===========================================================================

module RykDb.BlockCacheTests

open Xunit
open RykDb.Core

let private cacheEntry (fileNumber: int64) (blockId: int) (data: byte array) =
    {
        FileNumber = fileNumber
        BlockId = blockId
        Data = data
    }

let private count (cache: BlockCache) = List.length cache.Entries

[<Fact>]
let ``BlockCache: 入れたものは引ける`` () =
    let c = BlockCache.create 4
    let c1 = BlockCache.put c (cacheEntry 7L 0 [| 1uy; 2uy |])

    match BlockCache.get c1 7L 0 with
    | Some(c2, bytes) ->
        Assert.Equal<byte array>([| 1uy; 2uy |], bytes)
        Assert.Equal(1, count c2)
    | None -> Assert.Fail "入れたのに引けない"

[<Fact>]
let ``BlockCache: 上限を超えたら古い方から捨てる`` () =
    let c = BlockCache.create 2
    let c = BlockCache.put c (cacheEntry 1L 0 [| 1uy |])
    let c = BlockCache.put c (cacheEntry 2L 0 [| 2uy |])
    let c = BlockCache.put c (cacheEntry 3L 0 [| 3uy |])

    Assert.Equal(2, count c)
    Assert.False(BlockCache.contains c 1L 0)
    Assert.True(BlockCache.contains c 2L 0)
    Assert.True(BlockCache.contains c 3L 0)

[<Fact>]
let ``BlockCache: 読むと最近使った順に更新される`` () =
    let c = BlockCache.create 3
    let c = BlockCache.put c (cacheEntry 1L 0 [| 1uy |])
    let c = BlockCache.put c (cacheEntry 2L 0 [| 2uy |])
    let c = BlockCache.put c (cacheEntry 3L 0 [| 3uy |])

    // 1 番目を読む → 新しい順の先頭に移る
    let c2, _ = BlockCache.get c 1L 0 |> Option.get

    let c3 = BlockCache.put c2 (cacheEntry 4L 0 [| 4uy |])

    // 上限 3 なので「次に古い」2 番目が落ちる（1 番目は最近使ったので残る）
    Assert.True(BlockCache.contains c3 1L 0)
    Assert.False(BlockCache.contains c3 2L 0)

[<Fact>]
let ``BlockCache: ファイル単位に破棄できる`` () =
    let c = BlockCache.create 8
    let c = BlockCache.put c (cacheEntry 1L 0 [| 1uy |])
    let c = BlockCache.put c (cacheEntry 2L 0 [| 2uy |])
    let c = BlockCache.invalidateFile c 1L

    Assert.False(BlockCache.contains c 1L 0)
    Assert.True(BlockCache.contains c 2L 0)

[<Fact>]
let ``BlockCache: 容量 0 では何も保持しない`` () =
    let c0 = BlockCache.create 0
    let c = BlockCache.put c0 (cacheEntry 1L 0 [| 1uy |])

    Assert.Equal(0, count c)
    Assert.False(BlockCache.contains c 1L 0)
    Assert.True((BlockCache.get c 1L 0).IsNone)

    // 負の容量も 0 に丸める
    let c2 = BlockCache.put (BlockCache.create -5) (cacheEntry 1L 0 [| 1uy |])
    Assert.Equal(0, count c2)
    Assert.False(BlockCache.contains c2 1L 0)

[<Fact>]
let ``BlockCache: 同じキーの put は置き換えて最近使った位置へ移す`` () =
    let c = BlockCache.create 2
    let c = BlockCache.put c (cacheEntry 1L 0 [| 1uy |])
    let c = BlockCache.put c (cacheEntry 2L 0 [| 2uy |])
    // 1 を新しい内容で置き換える（2 が最も古くなる）
    let c = BlockCache.put c (cacheEntry 1L 0 [| 10uy |])
    let c = BlockCache.put c (cacheEntry 3L 0 [| 3uy |])

    // 容量 2 なので最も古い 2 が落ちる
    Assert.Equal(2, count c)
    Assert.Equal<byte array>([| 10uy |], BlockCache.get c 1L 0 |> Option.get |> snd)
    Assert.False(BlockCache.contains c 2L 0)
    Assert.True(BlockCache.contains c 3L 0)

[<Fact>]
let ``BlockCache: 存在しないキーの get は状態を変えない`` () =
    let c = BlockCache.create 4 |> fun c -> BlockCache.put c (cacheEntry 1L 0 [| 1uy |])

    Assert.True((BlockCache.get c 9L 9).IsNone)
    Assert.Equal(1, count c)
    Assert.True(BlockCache.contains c 1L 0)
