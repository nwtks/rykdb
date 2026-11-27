// ===========================================================================
// SSTable のファイル形式 — 定数・Footer・読み取りメタデータ。
//
//   [Block 0]...[Block N-1][Meta][Bloom][Index][Footer 84 bytes]
//
// 各ブロックは同じ接頭辞を持つ連続エントリのキー共有部分を圧縮する。
// ===========================================================================

namespace RykDb.Core

/// 索引の 1要素。
/// 各ブロックの位置と長さを張る。
type internal SstIndexEntry =
    {
        /// このブロックに含まれるキーのうち最大のもの
        LastKey: byte array
        Offset: int
        Length: int
    }

/// footer を decode した情報。
type internal SstFooterInfo =
    {
        Version: int
        MaxSeqNo: SeqNo
        IndexOff: int
        IndexLen: int
        BloomOff: int
        BloomLen: int
        MetaOff: int
        MetaLen: int
        EntryCount: int
        MetadataChecksum: uint32
    }

module internal SstableFooter =
    /// SSTable ファイルのマジック文字列。
    [<Literal>]
    let Magic = "RykDbSST"

    /// 書き出すファイルのバージョン（6）。
    /// ブロック末尾とメタデータ（meta / Bloom / index）・Footer を CRC32C で検証する。
    /// 全体チェックサムは持たず、部分読みでは読まなかったブロックは検証されない。
    [<Literal>]
    let private Version = 6

    /// 読み取れる下限バージョン。
    [<Literal>]
    let private MinReadableVersion = 6

    /// 最新 Footer の固定長（本体 80 + Footer チェックサム 4）。
    [<Literal>]
    let FooterSize = 84

    /// Footer で footerChecksum がカバーするバイト数（metadataChecksum フィールド込み）。
    [<Literal>]
    let private FooterChecksumLength = 80

    // Footer
    //   magic(8) version(4) maxSeqNo(8) indexOff(8) indexLen(8)
    //   bloomOff(8) bloomLen(8) metaOff(8) metaLen(8) entryCount(8)
    //   metadataChecksum(4) footerChecksum(4)
    //   = 8 + 4 + 8 * 8 + 4 + 4 = 84バイト（FooterSize と一致させる）
    // footerChecksum は先頭 80バイト（metadataChecksum を含む）のチェックサム。
    // 部分読みではこのフッターだけを検証できる。
    //
    // 全体 checksum は持たない。
    // データブロックはそれぞれ末尾のチェックサムで守られ、
    // meta / Bloom / index は metadataChecksum、Footer は footerChecksum で守られる。
    // 部分読みでは読まなかったブロックを検証できないが、全体 checksum があっても同じだった。
    //
    // maxSeqNo は起動時の NextSeqNo 復元のために必要。
    // ファイル内の最大 SeqNo を保存する
    //（GC は版ごとの SeqNo と active snapshot で判定する）。

    /// Footer を符号化する（末尾に自身のチェックサムを含む）。
    let encodeFooter
        (maxSeqNo: SeqNo)
        (indexOff: int)
        (indexLen: int)
        (bloomOff: int)
        (bloomLen: int)
        (metaOff: int)
        (metaLen: int)
        (entryCount: int)
        (metadataChecksum: uint32)
        =
        let body =
            Writer.build
                [
                    Writer.writeBytes (System.Text.Encoding.ASCII.GetBytes Magic)
                    Writer.uint32LE (uint32 Version)
                    Writer.int64LE (int64 maxSeqNo)
                    Writer.int64LE (int64 indexOff)
                    Writer.int64LE (int64 indexLen)
                    Writer.int64LE (int64 bloomOff)
                    Writer.int64LE (int64 bloomLen)
                    Writer.int64LE (int64 metaOff)
                    Writer.int64LE (int64 metaLen)
                    Writer.int64LE (int64 entryCount)
                    Writer.uint32LE metadataChecksum
                ]

        let footerChecksum = Hashing.checksum body
        Writer.build [ Writer.writeBytes body; Writer.uint32LE footerChecksum ]

    /// int に収まる非負の int64 か。
    let private fitsInt (v: int64) =
        v >= 0L && v <= int64 System.Int32.MaxValue

    /// Footer の先頭から各フィールドを読む
    /// （magic / version の検証は呼び出し側）。
    let private readFooterFields (footerBytes: byte array) =
        result {
            let c = Cursor.create footerBytes
            let! magicBytes, c = Reader.readBytes Magic.Length c
            let! version, c = Reader.readUInt32LE c
            let! maxSeqNo, c = Reader.readInt64LE c
            let! indexOff, c = Reader.readInt64LE c
            let! indexLen, c = Reader.readInt64LE c
            let! bloomOff, c = Reader.readInt64LE c
            let! bloomLen, c = Reader.readInt64LE c
            let! metaOff, c = Reader.readInt64LE c
            let! metaLen, c = Reader.readInt64LE c
            let! entryCount, c = Reader.readInt64LE c
            let! metadataChecksum, c = Reader.readUInt32LE c
            let! footerChecksum, _ = Reader.readUInt32LE c

            if Hashing.checksumRange footerBytes 0 FooterChecksumLength <> footerChecksum then
                return! err (DbError.corruption "Sstable: Footer のチェックサム不一致")
            else if
                not (fitsInt indexOff)
                || not (fitsInt indexLen)
                || not (fitsInt bloomOff)
                || not (fitsInt bloomLen)
                || not (fitsInt metaOff)
                || not (fitsInt metaLen)
                || not (fitsInt entryCount)
            then
                return! err (DbError.corruption "Sstable: Footer の値が大きすぎます")
            else
                return
                    System.Text.Encoding.ASCII.GetString magicBytes,
                    {
                        Version = int version
                        MaxSeqNo = uint64 maxSeqNo
                        IndexOff = int indexOff
                        IndexLen = int indexLen
                        BloomOff = int bloomOff
                        BloomLen = int bloomLen
                        MetaOff = int metaOff
                        MetaLen = int metaLen
                        EntryCount = int entryCount
                        MetadataChecksum = metadataChecksum
                    }
        }

    /// Footer の内容を検証する。
    let private validateFooter (magic: string) (footerStart: int64) (footer: SstFooterInfo) =
        if magic <> Magic then
            err (DbError.corruption "Sstable: マジックが違います")
        elif footer.Version < MinReadableVersion || footer.Version > Version then
            err (sprintf "Sstable: 未対応のバージョン %d" footer.Version |> DbError.corruption)
        elif
            footerStart < 0L
            || int64 footer.MetaOff + int64 footer.MetaLen <> int64 footer.BloomOff
            || int64 footer.BloomOff + int64 footer.BloomLen <> int64 footer.IndexOff
            || int64 footer.IndexOff + int64 footer.IndexLen <> footerStart
        then
            err (DbError.corruption "Sstable: Footer の範囲が不正です")
        else
            ret footer

    /// Footer バイト列を解析して検証する。
    let parseFooterBytes (footerStart: int64) (footerBytes: byte array) =
        readFooterFields footerBytes
        >>= fun (magic, footer) -> validateFooter magic footerStart footer
