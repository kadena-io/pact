- [Rest API](#sec-1)
  - [`cmd` フィールドとペイロード](#sec-1-1)
    - [`exec` ペイロード](#sec-1-1-1)
    - [`cont` ペイロード](#sec-1-1-2)
  - [エンドポイント](#sec-1-2)
    - [/send](#sec-1-2-1)
    - [/private](#sec-1-2-2)
    - [/poll](#sec-1-2-3)
    - [/listen](#sec-1-2-4)
    - [/local](#sec-1-2-5)
  - [API リクエスト フォーマッター](#sec-1-3)
    - [リクエストのYAML ファイル形式](#sec-1-3-1)
- [概念の説明](#sec-2)
  - [実行モード](#sec-2-1)
    - [コントラクト定義](#sec-2-1-1)
    - [トランザクション実行](#sec-2-1-2)
    - [クエリおよびローカル実行](#sec-2-1-3)
  - [データベース操作](#sec-2-2)
    - [アトミックな実行](#sec-2-2-1)
    - [キー/行モデル](#sec-2-2-2)
    - [クエリとパフォーマンス](#sec-2-2-3)
    - [Null 値なし](#sec-2-2-4)
    - [バージョン管理された履歴](#sec-2-2-5)
    - [バックエンド](#sec-2-2-6)
  - [型とスキーマ](#sec-2-3)
    - [実行時の型の施行](#sec-2-3-1)
    - [モジュール内の型推論](#sec-2-3-2)
    - [形式検証](#sec-2-3-3)
  - [キーセットと認証](#sec-2-4)
    - [キーセット定義](#sec-2-4-1)
    - [キーセット述語](#sec-2-4-2)
    - [キーの循環](#sec-2-4-3)
    - [モジュールのテーブル・ガード](#sec-2-4-4)
    - [行ごとのキーセット](#sec-2-4-5)
  - [計算モデル](#sec-2-5)
    - [チューリング不完全](#sec-2-5-1)
    - [変数への単一割り当て](#sec-2-5-2)
    - [データ型](#sec-2-5-3)
    - [パフォーマンス](#sec-2-5-4)
    - [制御フロー](#sec-2-5-5)
    - [関数型言語の概念](#sec-2-5-6)
    - [副作用のない実行](#sec-2-5-7)
    - [LISP](#sec-2-5-8)
    - [メッセージ データ](#sec-2-5-9)
  - [機密保持](#sec-2-6)
    - [エンティティ](#sec-2-6-1)
    - [互いに素なデータベース](#sec-2-6-2)
    - [機密保持のための pacts](#sec-2-6-3)
  - ["Pacts" による非同期トランザクションの自動化](#sec-2-7)
    - [パブリック pacts](#sec-2-7-1)
    - [プライベート pacts](#sec-2-7-2)
    - [失敗、ロールバック、キャンセル](#sec-2-7-3)
    - [イールドと再開](#sec-2-7-4)
    - [Pact 実行スコープと `pact-id`](#sec-2-7-5)
    - [pacts のテスト](#sec-2-7-6)
  - [モジュールの依存管理](#sec-2-8)
    - [モジュール ハッシュ](#sec-2-8-1)
    - [`use` を使用したモジュール バージョンの固定](#sec-2-8-2)
    - [インライン化された依存のモジュール](#sec-2-8-3)
    - [ハッシュの「ブレス」](#sec-2-8-4)
    - ["v2" モジュールによる段階的アップグレード](#sec-2-8-5)
- [シンタックス](#sec-3)
  - [リテラル](#sec-3-1)
    - [文字列](#sec-3-1-1)
    - [シンボル](#sec-3-1-2)
    - [整数](#sec-3-1-3)
    - [小数](#sec-3-1-4)
    - [ブール](#sec-3-1-5)
    - [リスト](#sec-3-1-6)
    - [オブジェクト](#sec-3-1-7)
    - [バインディング](#sec-3-1-8)
  - [型の指定](#sec-3-2)
    - [型リテラル](#sec-3-2-1)
    - [スキーマ型リテラル](#sec-3-2-2)
    - [型指定の対象](#sec-3-2-3)
  - [特殊形式](#sec-3-3)
    - [ドキュメントとメタデータ](#sec-3-3-1)
    - [bless](#sec-3-3-2)
    - [defun](#sec-3-3-3)
    - [defconst](#sec-3-3-4)
    - [defpact](#sec-3-3-5)
    - [defschema](#sec-3-3-6)
    - [deftable](#sec-3-3-7)
    - [let](#sec-3-3-8)
    - [let\*](#sec-3-3-9)
    - [step](#sec-3-3-10)
    - [step-with-rollback](#sec-3-3-11)
    - [use](#sec-3-3-12)
    - [module](#sec-3-3-13)
  - [式](#sec-3-4)
    - [アトム](#sec-3-4-1)
    - [S 式](#sec-3-4-2)
    - [参照](#sec-3-4-3)
- [時間形式](#sec-4)
  - [デフォルト形式と JSON のシリアル化](#sec-4-1)
  - [例](#sec-4-2)
    - [ISO8601](#sec-4-2-1)
    - [RFC822](#sec-4-2-2)
    - [YYYY-MM-DD hh:mm:ss.000000](#sec-4-2-3)
- [データベースのシリアル化形式](#sec-5)
  - [試行的なベータ版の機能に関する重要な警告](#sec-5-1)
  - [JSON 値によるキー値の形式](#sec-5-2)
  - [Pact データ型のコーデック](#sec-5-3)
    - [整数](#sec-5-3-1)
    - [小数](#sec-5-3-2)
    - [ブール](#sec-5-3-3)
    - [文字列](#sec-5-3-4)
    - [時刻](#sec-5-3-5)
    - [JSON 値/blob](#sec-5-3-6)
    - [キーセット](#sec-5-3-7)
  - [モジュール (ユーザー) テーブル](#sec-5-4)
    - [列名](#sec-5-4-1)
    - [ユーザー データ テーブル](#sec-5-4-2)
    - [ユーザー トランザクション テーブル](#sec-5-4-3)
- [組み込み関数](#sec-6)
  - [一般的な関数](#sec-6-1)
    - [at](#sec-6-1-1)
    - [bind](#sec-6-1-2)
    - [compose](#sec-6-1-3)
    - [constantly](#sec-6-1-4)
    - [contains](#sec-6-1-5)
    - [drop](#sec-6-1-6)
    - [enforce](#sec-6-1-7)
    - [enforce-one](#sec-6-1-8)
    - [enforce-pact-version](#sec-6-1-9)
    - [filter](#sec-6-1-10)
    - [fold](#sec-6-1-11)
    - [format](#sec-6-1-12)
    - [hash](#sec-6-1-13)
    - [identity](#sec-6-1-14)
    - [if](#sec-6-1-15)
    - [length](#sec-6-1-16)
    - [list](#sec-6-1-17)
    - [list-modules](#sec-6-1-18)
    - [make-list](#sec-6-1-19)
    - [map](#sec-6-1-20)
    - [pact-id](#sec-6-1-21)
    - [pact-version](#sec-6-1-22)
    - [read-decimal](#sec-6-1-23)
    - [read-integer](#sec-6-1-24)
    - [read-msg](#sec-6-1-25)
    - [remove](#sec-6-1-26)
    - [resume](#sec-6-1-27)
    - [reverse](#sec-6-1-28)
    - [sort](#sec-6-1-29)
    - [take](#sec-6-1-30)
    - [tx-hash](#sec-6-1-31)
    - [typeof](#sec-6-1-32)
    - [where](#sec-6-1-33)
    - [yield](#sec-6-1-34)
  - [データベース](#sec-6-2)
    - [create-table](#sec-6-2-1)
    - [describe-keyset](#sec-6-2-2)
    - [describe-module](#sec-6-2-3)
    - [describe-table](#sec-6-2-4)
    - [insert](#sec-6-2-5)
    - [keylog](#sec-6-2-6)
    - [keys](#sec-6-2-7)
    - [read](#sec-6-2-8)
    - [select](#sec-6-2-9)
    - [txids](#sec-6-2-10)
    - [txlog](#sec-6-2-11)
    - [update](#sec-6-2-12)
    - [with-default-read](#sec-6-2-13)
    - [with-read](#sec-6-2-14)
    - [write](#sec-6-2-15)
  - [時刻](#sec-6-3)
    - [add-time](#sec-6-3-1)
    - [days](#sec-6-3-2)
    - [diff-time](#sec-6-3-3)
    - [format-time](#sec-6-3-4)
    - [hours](#sec-6-3-5)
    - [minutes](#sec-6-3-6)
    - [parse-time](#sec-6-3-7)
    - [time](#sec-6-3-8)
  - [演算子](#sec-6-4)
    - [!=](#sec-6-4-1)
    - [\*](#sec-6-4-2)
    - [+](#sec-6-4-3)
    - [-](#sec-6-4-4)
    - [/](#sec-6-4-5)
    - [<](#sec-6-4-6)
    - [<=](#sec-6-4-7)
    - [=](#sec-6-4-8)
    - [>](#sec-6-4-9)
    - [>=](#sec-6-4-10)
    - [^](#sec-6-4-11)
    - [abs](#sec-6-4-12)
    - [and](#sec-6-4-13)
    - [and?](#sec-6-4-14)
    - [ceiling](#sec-6-4-15)
    - [exp](#sec-6-4-16)
    - [floor](#sec-6-4-17)
    - [ln](#sec-6-4-18)
    - [log](#sec-6-4-19)
    - [mod](#sec-6-4-20)
    - [not](#sec-6-4-21)
    - [not?](#sec-6-4-22)
    - [or](#sec-6-4-23)
    - [or?](#sec-6-4-24)
    - [round](#sec-6-4-25)
    - [sqrt](#sec-6-4-26)
  - [キーセット](#sec-6-5)
    - [define-keyset](#sec-6-5-1)
    - [enforce-keyset](#sec-6-5-2)
    - [keys-2](#sec-6-5-3)
    - [keys-all](#sec-6-5-4)
    - [keys-any](#sec-6-5-5)
    - [read-keyset](#sec-6-5-6)
  - [REPL 専用の関数](#sec-6-6)
    - [begin-tx](#sec-6-6-1)
    - [bench](#sec-6-6-2)
    - [commit-tx](#sec-6-6-3)
    - [env-data](#sec-6-6-4)
    - [env-entity](#sec-6-6-5)
    - [env-gas](#sec-6-6-6)
    - [env-gaslimit](#sec-6-6-7)
    - [env-gasprice](#sec-6-6-8)
    - [env-gasrate](#sec-6-6-9)
    - [env-hash](#sec-6-6-10)
    - [env-keys](#sec-6-6-11)
    - [env-step](#sec-6-6-12)
    - [expect](#sec-6-6-13)
    - [expect-failure](#sec-6-6-14)
    - [json](#sec-6-6-15)
    - [load](#sec-6-6-16)
    - [pact-state](#sec-6-6-17)
    - [print](#sec-6-6-18)
    - [rollback-tx](#sec-6-6-19)
    - [sig-keyset](#sec-6-6-20)
    - [typecheck](#sec-6-6-21)
    - [verify](#sec-6-6-22)
- [Pact のプロパティ チェック システム](#sec-7)
  - [概要](#sec-7-1)
  - [プロパティおよびスキーマの不変条件の記述](#sec-7-2)
  - [プロパティ チェッカーのしくみ](#sec-7-3)
  - [プロパティ チェッカーの使用方法](#sec-7-4)
  - [プロパティの表現](#sec-7-5)
    - [引数、戻り値、標準の演算子、比較演算子](#sec-7-5-1)
    - [ブール演算子](#sec-7-5-2)
    - [トランザクションの中止と成功](#sec-7-5-3)
    - [プロパティ API の詳細](#sec-7-5-4)
  - [スキーマの不変条件の表現](#sec-7-6)
    - [キーセットの認証](#sec-7-6-1)
    - [データベース アクセス](#sec-7-6-2)
    - [質量保存の法則と列の差分](#sec-7-6-3)
    - [全称限定子と存在限定子](#sec-7-6-4)
    - [プロパティの定義と再使用](#sec-7-6-5)
  - [単純な残高の移動の例](#sec-7-7)
- [プロパティと不変条件の関数](#sec-8)
  - [数値演算子](#sec-8-1)
    - [+](#sec-8-1-1)
    - [-](#sec-8-1-2)
    - [\*](#sec-8-1-3)
    - [/](#sec-8-1-4)
    - [^](#sec-8-1-5)
    - [log](#sec-8-1-6)
    - [-](#sec-8-1-7)
    - [sqrt](#sec-8-1-8)
    - [ln](#sec-8-1-9)
    - [exp](#sec-8-1-10)
    - [abs](#sec-8-1-11)
    - [round](#sec-8-1-12)
    - [ceiling](#sec-8-1-13)
    - [floor](#sec-8-1-14)
    - [mod](#sec-8-1-15)
  - [論理演算子](#sec-8-2)
    - [>](#sec-8-2-1)
    - [<](#sec-8-2-2)
    - [>=](#sec-8-2-3)
    - [<=](#sec-8-2-4)
    - [=](#sec-8-2-5)
    - [!=](#sec-8-2-6)
    - [and](#sec-8-2-7)
    - [or](#sec-8-2-8)
    - [not](#sec-8-2-9)
    - [when](#sec-8-2-10)
  - [オブジェクト演算子](#sec-8-3)
    - [at](#sec-8-3-1)
    - [+](#sec-8-3-2)
  - [文字列演算子](#sec-8-4)
    - [length](#sec-8-4-1)
    - [+](#sec-8-4-2)
  - [時間演算子](#sec-8-5)
    - [add-time](#sec-8-5-1)
  - [定量演算子](#sec-8-6)
    - [forall](#sec-8-6-1)
    - [exists](#sec-8-6-2)
  - [トランザクション演算子](#sec-8-7)
    - [abort](#sec-8-7-1)
    - [success](#sec-8-7-2)
    - [result](#sec-8-7-3)
  - [データベース演算子](#sec-8-8)
    - [table-written](#sec-8-8-1)
    - [table-read](#sec-8-8-2)
    - [cell-delta](#sec-8-8-3)
    - [column-delta](#sec-8-8-4)
    - [column-written](#sec-8-8-5)
    - [column-read](#sec-8-8-6)
    - [row-read](#sec-8-8-7)
    - [row-written](#sec-8-8-8)
    - [row-read-count](#sec-8-8-9)
    - [row-write-count](#sec-8-8-10)
  - [許可演算子](#sec-8-9)
    - [authorized-by](#sec-8-9-1)
    - [row-enforced](#sec-8-9-2)

本書は、[高性能なブロックチェーン](http://kadena.io/)上で正しいトランザクション実行を実現するために設計された Pact スマートコントラクト言語の参考資料です。詳細な背景情報については、[ホワイトペーパー](http://kadena.io/docs/Kadena-PactWhitepaper.pdf)または [pact のホーム ページ](http://kadena.io/pact)を参照してください。

Copyright (c) 2016 - 2018, Stuart Popejoy. All Rights Reserved.

# Rest API<a id="sec-1"></a>

Pact のバージョン 2.1.0 以降には HTTP サーバーと SQLite バックエンドが組み込まれています。このため、 `pact` ツールだけで、ブロックチェーンアプリケーションのプロトタイプを作成できます。

サーバーを起動するには、適切な構成を使用して `pact -s config.yaml` を発行します。 `pact-lang-api` JS ライブラリは、Web 開発用の [npm 経由で提供されています](https://www.npmjs.com/package/pact-lang-api)。

## `cmd` フィールドとペイロード<a id="sec-1-1"></a>

ブロックチェーンに送信されたペイロードは、受信されたコマンドが正しいことを確認するために、 ハッシュされる必要があります。 これはまた、必要なプライベートキーによって署名される値でも あります。トランザクションの JSON を、ハッシュを構成している値とバイト単位で一致させるには、 JSON をペイロード内に文字列として "エンコード" (つまり、["文字列化"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)) する必要があります。 `cmd` フィールドは、 `exec` ペイロードと `cont` ペイロードの 2 種類のペイロードをサポートしています。

### `exec` ペイロード<a id="sec-1-1-1"></a>

`exec` ペイロードには、実行可能コードとデータが、エンコードされた文字列として保持されます。[send](https://pact-language.readthedocs.io/en/latest/pact-reference.html#send)、[private](https://pact-language.readthedocs.io/en/latest/pact-reference.html#private)、 および [local](https://pact-language.readthedocs.io/en/latest/pact-reference.html#local) の各エンドポイントは、 `cmd` フィールドでこのペイロード型をサポートします。エンコードされる JSON の形式は次のとおりです。

```js
{
    "nonce": "[ナンス値 (すべての呼び出しにおいてユニークな値)]",
    "payload": {
        "exec": {
            "code": "[実行する pact コード]",
            "data": { /* ユーザーのデータ */ }
        }
    }
}
```

メッセージをアセンブルするときには、この JSON が "文字列化" され、 `cmd` フィールドに指定されている必要があります。[pact ツールでリクエストフォーマッター](https://pact-language.readthedocs.io/en/latest/pact-reference.html#api-request-formatter)の出力を確認すると、 `cmd` フィールドは、指定されたコードとともに、エンコードされ、エスケープされた JSON の文字列であることがわかります。

### `cont` ペイロード<a id="sec-1-1-2"></a>

`cont` ペイロードを使用すると、[pact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#pacts) を継続またはロール バックすることができます。このペイロードには、関係する pact の ID、pact をロールバックするのか継続するのかの指定、ステップ番号、必要なステップデータの各フィールドを要します。 これらのペイロードフィールドには、以下の特別な制約があります。

-   pact ID は、pact をインスタンス化したトランザクションの ID と同じです。
-   1 つのトランザクションでインスタンス化できるのは、1 つの pact のみです。
-   pact をロールバックする場合、ステップ番号は直前に実行されたステップの番号を指定します。
-   pact を継続する場合、ステップ番号には直前に実行されたステップに 1 を加えた数値を指定します。

`exec` ペイロード フィールドと同様、 `cont` ペイロード フィールドも文字列としてエンコードする必要があります。[send](https://pact-language.readthedocs.io/en/latest/pact-reference.html#send) エンドポイントは、 `cmd` フィールドでこのペイロード型をサポートします。 エンコードされる JSON の形式は次のとおりです。

```js
{
    "nonce": "[ナンス値 (すべての呼び出しにおいてユニークな値)]",
    "payload": {
        "cont": {
            "txid": [インスタンス化される pact コードが含まれるトランザクション id]
            "rollback": [true *or* false],
            "step": [継続する、またはロールバックするステップの ID]
            "data": { /* ユーザーのデータ */ }
        }
    }
}
```

## エンドポイント<a id="sec-1-2"></a>

すべてのエンドポイントは、 `api/v1` から運用されます。したがって `localhost:8080` で実行されている場合、send 呼び出しは [http://localhost:8080/api/v1/send](http://localhost:8080/api/v1/send) に送信します。

### /send<a id="sec-1-2-1"></a>

1 つ以上の "public" (暗号化されていない) コマンドをブロックチェーンに非同期送信します。文字列化された JSON データについては、 [cmd フィールド形式](https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads) を参照してください。

リクエスト JSON:

```js
{
    "cmds": [
        {
            "hash": "[base16 の blake2 ハッシュ]",
            "sigs": [
                {
                    "sig": "[‘hash' に一致するプライベートキーによる署名]",
                    "pubKey": "[base16 のパブリックキー]",
                    "scheme": "ED25519" /* 任意 (デフォルトは ED25519) */
                }
            ]
            "cmd": "[JSON 化されたトランザクションコード]"
        }
        // ... 次のコマンド
    ]
}
```

レスポンス JSON:

```js
{
  "status": "success|failure",
  "response": {
    "requestKeys": [
      "[成功の証明としてリクエストで用いたハッシュ]"
    ]
  }
}
```

### /private<a id="sec-1-2-2"></a>

指定されたアドレス情報を使用して、1 つ以上の "private" コマンドをブロックチェーンに非同期送信することで、送受信するエンティティのみが読み取られるようにセキュアに暗号化します。 文字列化された JSON データについては、[cmd フィールド形式](https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads)を参照してください。

リクエスト JSON:

```js
{
    "cmds": [
        {
            "hash": "[base16 の blake2 ハッシュ]",
            "sigs": [
                {
                    "sig": "[‘hash' に一致するプライベートキーによる署名]",
                    "pubKey": "[base16 のパブリックキー]",
                    "scheme": "ED25519" /* 任意 (デフォルトは ED25519)*/
                }
            ]
            "cmd": "[JSON化 されたトランザクションコード]"
        }
    ]
}
```

レスポンス JSON:

```js
{
  "status": "success|failure",
  "response": {
    "requestKeys": [
      "[成功の証明としてリクエストで用いたハッシュ]"
    ]
  }
}
```

### /poll<a id="sec-1-2-3"></a>

コマンドの結果をポーリングします。

リクエスト JSON:

```json
{
  "requestKeys": [
    "[クエリしたいコマンドのハッシュ]"
  ]
}
```

レスポンス JSON:

```js
{
    "status": "success|failure",
    "response": {
        "[コマンドのハッシュ]": {
            "result": {
                "status": "success|failure",
                "data": /* トランザクション実行のデータ */
            },
            "txId": /* トランザクションのID (トランザクションの履歴などのクエリに使用) */
        }
    }
}
```

### /listen<a id="sec-1-2-4"></a>

単一のコマンド結果、または既に実行されたコマンドを取得するために、ブロッキングリクエストをします。

リクエスト JSON:

```js
{
  "listen": "[コマンドのハッシュ]"
}
```

レスポンス JSON:

```js
{
    "status": "success|failure",
    "response": {
        "result": {
            "status": "success|failure",
            "data": /* トランザクション実行のデータ */
        },
        "txId": /* トランザクションのID (トランザクションの履歴などのクエリに使用) */
    }
}
```

### /local<a id="sec-1-2-5"></a>

トランザクション以外の実行コマンドを送信するブロッキングおよび同期呼び出しです。 ブロックチェーン環境では、これはノードローカルの "ダーティリード" です。データベースへの書き込みや変更は、ロールバックされます。文字列化された JSON データについては、[cmd フィールド形式](https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads)を参照してください。

リクエスト JSON:

```js
{
    "hash": "[base16 の blake2 ハッシュ]",
    "sigs": [
        {
            "sig": "[‘hash' に一致するプライベートキーによる署名]",
            "pubKey": "[base16のパブリックキー]",
            "scheme": "ED25519" /* 任意 (デフォルトは ED25519)*/
        }
    ]
    "cmd": "[stringified transaction JSON]"
}
```

レスポンス JSON:

```js
{
    "status": "success|failure",
    "response": {
        "status": "success|failure",
        "data": /*トランザクション実行のデータ*/
    }
}
```

## API リクエスト フォーマッター<a id="sec-1-3"></a>

Pact 2.2.3 では、 `pact` ツールで `-a` オプションを使用できるようになりました。これにより、リクエストを記述した YAML ファイルを使用して API リクエスト JSON をフォーマットできます。出力は Postman などの POST ツールで使用でき、 `curl` にパイプすることも可能です。

例えば、以下の内容を持つ "apireq.yaml" という yaml ファイルがあるとします。

```yaml
code: "(+ 1 2)"
data:
  name: Stuart
  language: Pact
keyPairs:
  - public: ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d
    secret: 8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332
```

このファイルは、 `pact` に入力されて、次のような有効な API リクエストを取得できます。

```
$ pact -a tests/apireq.yaml -l
{"hash":"444669038ea7811b90934f3d65574ef35c82d5c79cedd26d0931fddf837cccd2c9cf19392bf62c485f33535983f5e04c3e1a06b6b49e045c5160a637db8d7331","sigs":[{"sig":"9097304baed4c419002c6b9690972e1303ac86d14dc59919bf36c785d008f4ad7efa3352ac2b8a47d0b688fe2909dbf392dd162457c4837bc4dc92f2f61fd20d","scheme":"ED25519","pubKey":"ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"}],"cmd":"{\"address\":null,\"payload\":{\"exec\":{\"data\":{\"name\":\"Stuart\",\"language\":\"Pact\"},\"code\":\"(+ 1 2)\"}},\"nonce\":\"\\\"2017-09-27 19:42:06.696533 UTC\\\"\"}"}
```

以下に、curl にパイプし、ポート 8080 で実行されている pact サーバーに接続する例を示します。

```
$ pact -a tests/apireq.yaml -l | curl -d @- http://localhost:8080/api/v1/local
{"status":"success","response":{"status":"success","data":3}}
```

### リクエストのYAML ファイル形式<a id="sec-1-3-1"></a>

リクエスト yaml ファイルには、2 つの形式があります。"実行" リクエスト yaml ファイルは、[exec](https://pact-language.readthedocs.io/en/latest/pact-reference.html#exec-payload) ペイロードを記述します。これに対し、"継続"リクエスト yaml ファイルは、[cont](https://pact-language.readthedocs.io/en/latest/pact-reference.html#cont-payload) ペイロードを記述します。

実行リクエスト yaml では次のキーを指定できます。

```yaml
code: トランザクションコード
codeFile: トランザクションコードのファイル
data: JSON 化されたトランザクションデータ
dataFile: JSON 化されたトランザクションデータのファイル
keyPairs: 署名のためのキーペア (発生させるには pact -g): [
  public: base 16 のパブリックキー
  secret: base 16 のプライベートキー
  ]
nonce: 任意のナンス (提供しない場合は現在時刻が使われる)
from: プライベートメッセージ用のエンティティ名
to: プライベートメッセージ用のエンティティ名
```

継続リクエスト yaml では次のキーを指定できます。

```yaml
type: "cont"
txId: pact のトランザクション ID (整数)
step: 次のステップのID (整数)
rollback: この pact はロールバック可能かどうか (ブール)
data: トランザクションデータの JSON
dataFile: トランザクションデータの JSON のファイル
keyPairs: 署名のためのキーペア (発生させるにはpact -g): [
 public: base 16 のパブリックキー
 secret: base 16 のプライベートキー
 ]
nonce: 任意のナンス (提供しない場合は現在時刻が使われる)
from: プライベートメッセージ用のエンティティ名
to: プライベートメッセージ用のエンティティ名
```

# 概念の説明<a id="sec-2"></a>

## 実行モード<a id="sec-2-1"></a>

Pact言語 は、独自の "実行モード" で使用して、ブロックチェーン上での高速リニア実行の パフォーマンス要件に対応するように設計されています。これらの実行モードは次のとおりです。

1.  コントラクト定義
2.  トランザクション実行
3.  クエリおよびローカル実行

### コントラクト定義<a id="sec-2-1-1"></a>

このモードでは、コードがブロックチェーンに送信されて、コード (モジュール)、テーブル (データ)、キーセット (認証) から構成されるスマートコントラクトを確立します。これには、例えばデータを初期化するための (データベースを変更する) トランザクションコードも含まれます。

特定のスマートコントラクトでは、これらはすべて単一のメッセージとしてブロックチェーンに送信されます。 したがって、エラーが生じた場合、スマートコントラクト全体がまとめてロールバックされます。

1.  キーセット定義

    [キーセット](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets)は、モジュールとテーブルの管理者認証方式の指定に使用されるため、 通常はファイルの一番最初に定義されます。定義によってランタイム環境にキーセットが作成され、 グローバル キーセット データベースにそれらの定義が格納されます。

2.  モジュールの宣言

    [モジュール](https://pact-language.readthedocs.io/en/latest/pact-reference.html#module)には、スマート コントラクトの API とデータの定義が含まれます。これには以下のもので構成されています。

    -   [関数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defun)
    -   [スキーマ](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defschema)定義
    -   [テーブル](https://pact-language.readthedocs.io/en/latest/pact-reference.html#deftable)定義
    -   [pact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defpact) の特殊関数
    -   [const](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defconst) 値

    モジュールが宣言されると、他のモジュールからのネイティブ関数や定義への参照がすべて解決されます。 解決が失敗すると、結果としてトランザクションがロールバックします。

    モジュールは、管理キーセットによって制御して再定義できます。モジュールのバージョン管理は、 モジュール名にバージョン番号を含める方法以外サポートされていません ("accounts-v1" など)。もっとも、"モジュール ハッシュ" が、コードの安全性を確保するための強力な機能を備えています。モジュールを [use](https://pact-language.readthedocs.io/en/latest/pact-reference.html#use) によってインポートするときには、モジュール ハッシュを指定して、コードを特定のリリースに関連付けることができます。

    Pact 2.2 以降では、モジュール宣言内で `use` ステートメントを発行できます。 この機能をモジュール ハッシュと組み合わせると、依存モジュールが後からチェーン上で 変更された場合に、更新されたモジュールコードがインポートに失敗することで高水準の保証が実現します。 またロードされたモジュールのハッシュに変更内容が伝達されるため、更新が行われても、 ダウンストリームのモジュールが誤って変更されることはありません。

    モジュール名は、グローバルに一意である必要があります。

3.  テーブルの作成

    テーブルは、モジュールと同時に[作成](https://pact-language.readthedocs.io/en/latest/pact-reference.html#create-table)されます。テーブルはモジュール内で "定義" されますが、モジュールの作成後に "作成" されるため、テーブルを必ずしも再作成しなくても、後からモジュールを再定義できます。

    モジュールのテーブルに対する関係は重要です。これについては、「[テーブルのガード](https://pact-language.readthedocs.io/en/latest/pact-reference.html#module-table-guards)」で説明します。

    作成できるテーブルの数に制限はありません。テーブル名は、モジュール名を使用して名前空間化されます。

    テーブルは[スキーマ](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defschema)によって型指定できます。

### トランザクション実行<a id="sec-2-1-2"></a>

"トランザクション" とは、支払、販売、複雑な契約のワークフロー ステップなど、ブロックチェーン上で行われるビジネス イベントを指します。一般にトランザクションは、モジュール関数への 1 回の呼び出しです。ただし、実行するステートメントの数に制限はありません。実際、"トランザクション" と "スマート コントラクトの定義" の違いは、単に実行されるコードの "種類" の問題に過ぎず、コードの評価における実際的な違いはありません。

### クエリおよびローカル実行<a id="sec-2-1-3"></a>

データのクエリは、通常はビジネス イベントではなく、しかもパフォーマンスに影響を与えるデータ ペイロードが含まれることがあります。そのためクエリは、メッセージの受信側ノード上で "ローカル実行" として処理されます。履歴クエリでは、"トランザクション ID" が参照点として使用されるため、あらゆる競合状態が回避され、非同期でクエリを実行できます。

トランザクション実行とローカル実行の違いは、異なる API エンドポイントをターゲットとすることで実現されます。pact コードは、トランザクション実行とローカル実行を区別できません。

## データベース操作<a id="sec-2-2"></a>

Pact 言語は、ブロックチェーンの実行に特有な必須条件をそなえたデータベースのようなものだとも言えます。 Pact 言語は、さまざまなバックエンドに適応して実行できます。

### アトミックな実行<a id="sec-2-2-1"></a>

ブロックチェーン内に送信され、Pact によって実行されるメッセージは常に "アトミック"です。トランザクションはその全体が成功するか、全体が失敗するかのいずれかです。これはデータベース用語で言うところの "トランザクション" と同じです。ロールバック処理は、[マルチステップのトランザクション](https://pact-language.readthedocs.io/en/latest/pact-reference.html#pacts)を除いて、明示的なサポートはありません。

### キー/行モデル<a id="sec-2-2-2"></a>

ブロックチェーンの実行では、OLTP (オンライン トランザクション処理) データベースのワークロードと同様、非正規化データを単一のテーブルに書き込みます。Pact のデータアクセス API には、これを反映した "キー/行モデル" が搭載されています。このモデルでは、1 つのキーによって、1 つの列にアクセスします。

そのため、Pact 言語ではテーブルの ”結合” (join) はサポートされていません。テーブルの結合を行う場合は、 Pact データベースからエクスポートしたデータを使った OLAP (オンライン分析処理) データベースでの分析が適しています。しかし、Pact がリレーショナルな手法でトランザクションを "記録" できないわけではありません。例えば Customer テーブルのキーが Sales テーブルで使用されている場合に、Customer テーブルのコードによって Customer レコードを検索して Sales テーブルに書き込むことができます。

### クエリとパフォーマンス<a id="sec-2-2-3"></a>

Pact 2.3 以降の Pact には、テーブルから複数の行を選択する強力なクエリの仕組みが搭載されています。 これは一見 SQL に似ていますが、[select](https://pact-language.readthedocs.io/en/latest/pact-reference.html#select) 演算と [where](https://pact-language.readthedocs.io/en/latest/pact-reference.html#where) 演算によってテーブルへの "ストリーミング インターフェイス" が提供されます。そこではユーザーがフィルター関数を指定した後、[sort](https://pact-language.readthedocs.io/en/latest/pact-reference.html#sort) やその他の関数を使用して行セットをリスト データ構造として操作します。

```lisp
;; 給料が 90000 以上の開発者を選び、年齢によって並び替えます

(reverse (sort ['age]
  (select 'employees ['first-name,'last-name,'age]
    (and? (where 'title (= "Programmer"))
          (where 'salary (< 90000))))))

;; 'filter' でも同じクエリが可能です

(reverse (sort ['age]
  (filter (and? (where 'title (= "Programmer"))
                (where 'salary (< 90000)))
          employees)))
```

トランザクション環境において、Pact のデータベース操作は、1 行単位の読み書きに最適化されています。つまり、上の例のクエリの計算速度やコストを予想できなくなることがあります。しかし、[ローカル](https://pact-language.readthedocs.io/en/latest/pact-reference.html#local)実行機能を使用すれば、Pact がストリーミング結果に対してユーザーのフィルター関数を利用できるため、優れたパフォーマンスが発揮されます。

したがって、ローカルの非トランザクション操作によって選択操作を行い、トランザクション環境では大きなテーブルで選択を使用しないようにすることがベスト プラクティスとなります。

### Null 値なし<a id="sec-2-2-4"></a>

Pact言語のデータベース機能には、NULL 値の概念がありません。列の値が 1 つでも見つからない場合は、データベース結果に対する計算の主要な関数である [with-read](https://pact-language.readthedocs.io/en/latest/pact-reference.html#with-read) がエラーを出します。トランザクションの作成者は、トランザクションのあらゆる読み取りに対して値が存在するように注意する必要があります。これは "全体性" を確保し、null 値に関わる不必要で危険な制御フローを回避するための安全機能です。

### バージョン管理された履歴<a id="sec-2-2-5"></a>

さらにこのキー/行モデルでは、列の値が変更されるたび、その変更がトランザクション ID によってバージョン管理されます。例えば、"name"、"age"、"role" という 3 列構成のテーブルがあるとして、第一トランザクションでは "name" を、第二トランザクションでは "age" と "role" を更新したとします。履歴データを取得すると、第一トランザクションでは "name" に対する変更のみが、第二トランザクションでは "age" と "role" への変更のみが返されます。

### バックエンド<a id="sec-2-2-6"></a>

Pact では、ブロックチェーン内のスマートコントラクト レイヤーで、同一の正確な実行が保証されます。そのため、バッキング ストアは、異なるコンセンサス ノード上で同一である必要がありません。Pact を実装すると、ダウンストリーム・システムにデータを容易に一括レプリケーションできるため、産業用 RDBMS の統合が可能になり、ブロックチェーンに基づいたシステムへの大規模な移行が促進されます。

## 型とスキーマ<a id="sec-2-3"></a>

Pact 2.0 以降では、随意ではありますが、明示的な型指定ができます。 型なしの Pact 1.0 コードは以前と同様に機能します。型なしのコード作成は、手早くプロトタイプを作成したい場合に便利です。

スキーマは、型指定の一番の動機となります。スキーマは、型指定できる列のリストを使用して[定義](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defschema)されます (型は必須ではありません)。次にテーブルが、特定のスキーマを使用して[定義](https://pact-language.readthedocs.io/en/latest/pact-reference.html#deftable)されます (これもオプションです)。

スキーマは、オブジェクト型についても使用でき、また指定できます。

### 実行時の型の施行<a id="sec-2-3-1"></a>

コードで宣言されたすべての型は、ランタイムで施行されます。テーブル スキーマの場合は、テーブルへのすべての書き込みが、スキーマに対して型チェックされます。そうでない場合は、型指定が検出されると、式の評価時にランタイムがその型を施行します。

### モジュール内の型推論<a id="sec-2-3-2"></a>

[typecheck](https://pact-language.readthedocs.io/en/latest/pact-reference.html) という repl コマンド を使用すると、Pact インタープリターによってモジュールが分析され、すべての変数、関数適用、定数定義に対して型推論が試されます。プロジェクトの repl スクリプトでこれを使用すると、開発者が型チェックに成功するための ”必要最小限の型指定” を追加する作業が軽減されます。型チェックに問題なく成功するには、通常、 すべてのテーブルに対してスキーマが指定され、曖昧なまたは多重定義されたネイティブ関数を呼び出す補助関数で引数の型が指定されている必要があります。

### 形式検証<a id="sec-2-3-3"></a>

SMT-LIB2 言語の証明を自動的に作り出すため、Pact は完全に型チェックされインラインもされた AST を出力します。型チェックが成功しない場合、モジュールを証明できなくなります。

このように、Pact コードは、型無しから、"十分な" 型指定、さらには形式検証に至るまで、段階的に "安全性" を高めることができます。

Pact 2.0 では、形式検証がまだ開発中であることに注意してください。

## キーセットと認証<a id="sec-2-4"></a>

Pact では、Bitcoin スクリプトと同様、パブリックキー認証がスマート コントラクトの実行と管理に直接組み込まれています。

### キーセット定義<a id="sec-2-4-1"></a>

キーセットは、メッセージのペイロードから定義を[読み取る](https://pact-language.readthedocs.io/en/latest/pact-reference.html#read-keyset)ことによって[定義](https://pact-language.readthedocs.io/en/latest/pact-reference.html#define-keyset)されます。キーセットは、パブリックキーと "キーセット述語" のリストから構成されます。

有効なキーセット JSON の作成例を以下に示します。

```js
{
    "fully-specified-with-native-pred":
      { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "keys-2" },

    "fully-specified-with-qual-custom":
      { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred":
      "my-module.custom-pred" },

    "keysonly":
      { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"] }, /* デフォルトは "keys-all" */

    "keylist": ["abc6bab9b88e08d","fe04ddd404feac2"] /* "keys-all" 述語のキーセットを作成 */
}
```

### キーセット述語<a id="sec-2-4-2"></a>

キーセット述語はある特定の関数を参照します。選ばれた関数は、キーセット内のパブリックキーと、ブロックチェーン メッセージの署名に使用されたキーを比較します。これが一致しない場合は、トランザクションが進みません。この関数では、"count" と "matched" の 2 つの引数を指定できます。"count" はキーセット内のキーの数、"matched" はメッセージの署名に使用されているキーのうち、キーセットのキーと一致するキーの数です。

複数署名のサポートは、ブロックチェーン レイヤーの責任であり、Bitcoin 型の "マルチシグ" コントラクト (決済には少なくとも 2 つの署名が必要) が持つ強力な機能です。

Pact には、[keys-all](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-all)、[keys-any](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-any)、[keys-2](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-2) のキーセット述語が組み込まれています。 モジュールの作成者は、追加の述語を自由に定義できます。

キーセット述語が指定されていない場合、デフォルトで [keys-all](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-all) が使用されます。

### キーの循環<a id="sec-2-4-3"></a>

キーセットは循環させることができます。ただし、現在のキーセット定義とキーセット述語に対して認証されたメッセージよってのみ可能です。認証が完了した後は、キーセットを簡単に[再定義](https://pact-language.readthedocs.io/en/latest/pact-reference.html#define-keyset)できます。

### モジュールのテーブル・ガード<a id="sec-2-4-4"></a>

テーブルを[作成](https://pact-language.readthedocs.io/en/latest/pact-reference.html#create-table)するときには、モジュール名を併せて指定する必要があります。この仕組みにより、[データアクセス関数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#Database)を介したテーブルへの直接アクセスが、モジュールの管理キーセットによって認証されることで、テーブルがモジュールによって ”保護”されます。ただし、モジュール関数内では、テーブル アクセスは制約されません。これにより、コントラクト作成者はきわめて柔軟にデータ アクセスを設計できるため、モジュールを主な "ユーザー" データ アクセス API として位置付けることができます。

### 行ごとのキーセット<a id="sec-2-4-5"></a>

キーセットは列の値として行に格納して、行全体を認証できます。以下のコードでは、これを実現する方法を示します。

```lisp
(defun create-account (id)
  (insert accounts id { "balance": 0.0, "keyset": (read-keyset "owner-keyset") }))

(defun read-balance (id)
  (with-read accounts id { "balance":= bal, "keyset":= ks }
    (enforce-keyset ks)
    (format "Your balance is {}" [bal])))
```

この例では、 `create-account` が [read-keyset](https://pact-language.readthedocs.io/en/latest/pact-reference.html#read-keyset) を使用してメッセージのペイロードからキーセット定義を読み取り、テーブルに "keyset" として格納します。 `read-balance` は、最初に [enforce-keyset](https://pact-language.readthedocs.io/en/latest/pact-reference.html#enforce-keyset) でキーセットを適用することで、持ち主のキーセットに対し、残高の読み取りのみを許可します。

## 計算モデル<a id="sec-2-5"></a>

ここでは、Pact の計算手法について説明します。

### チューリング不完全<a id="sec-2-5-1"></a>

Pact はチューリング不完全です。つまり、再帰機能がなく (再帰は実行前に検出されればエラーとなります)、無限にループすることもできません。 Pact は、[map](https://pact-language.readthedocs.io/en/latest/pact-reference.html#map)、[fold](https://pact-language.readthedocs.io/en/latest/pact-reference.html#fold)、および [filter](https://pact-language.readthedocs.io/en/latest/pact-reference.html#filter) を介してリスト構造上での演算をサポートしますが、 無限のリストを定義できないため、これらには必然的に限界があります。

Pact モジュールは、チューリング不完全であるため、すべての参照が事前にロードされて解決されます。つまり、ルックアップ テーブルで関数を読み取るのではなく、関数のコードそのものがコールサイトに直接挿入されます。これはチューリング不完全な言語のパフォーマンス上の利点の一例です。

### 変数への単一割り当て<a id="sec-2-5-2"></a>

Pact では、[let expressions](https://pact-language.readthedocs.io/en/latest/pact-reference.html#let) と [bindings](https://pact-language.readthedocs.io/en/latest/pact-reference.html#bindings) で変数を宣言できます。変数は不変です。再割り当てやインプレースでの変更はできません。

一般に、変数宣言は [with-read](https://pact-language.readthedocs.io/en/latest/pact-reference.html#with-read) 関数で行われ、変数が名前によって列の値に割り当てられます。 [bind](https://pact-language.readthedocs.io/en/latest/pact-reference.html#bind) 関数はこれと同じ機能をオブジェクトに対して提供します。

モジュールグローバルな定数値は、[defconst](https://pact-language.readthedocs.io/en/latest/pact-reference.html) で宣言できます。

### データ型<a id="sec-2-5-3"></a>

Pact コードに型を付けるのはユーザーの自由です。型の指定がなくても型チェックは行われるので、指定すればする程コードの安全性が高まります。

Pact 言語は次の型をサポートしています。

-   [文字列](https://pact-language.readthedocs.io/en/latest/pact-reference.html#strings)
-   [整数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#integers)
-   [小数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#decimals)
-   [ブール](https://pact-language.readthedocs.io/en/latest/pact-reference.html#booleans)
-   [キーセット](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets)
-   [リスト](https://pact-language.readthedocs.io/en/latest/pact-reference.html#lists)
-   [オブジェクト](https://pact-language.readthedocs.io/en/latest/pact-reference.html#objects)
-   [関数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defun)と [pact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defpact) 定義
-   [JSON 値](https://pact-language.readthedocs.io/en/latest/pact-reference.html#json)
-   [テーブル](https://pact-language.readthedocs.io/en/latest/pact-reference.html#deftable)
-   [スキーマ](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defschema)

### パフォーマンス<a id="sec-2-5-4"></a>

Pact 言語は、ブロックチェーンでのビジネス イベントの記録を迅速化するためにクエリとモジュール定義にペナルティを課して、[トランザクション実行](https://pact-language.readthedocs.io/en/latest/pact-reference.html#transaction-execution)のパフォーマンスを最優先するように設計されています。以下に、高速で実行するためのヒントを示します。

1.  単一関数のトランザクション

    単一の関数呼び出しで実行できるようにトランザクションを設計しましょう。

2.  `use` ではなく参照を使用した呼び出し

    トランザクションでモジュール関数を呼び出すときは、[use](https://pact-language.readthedocs.io/en/latest/pact-reference.html#use) でモジュールをインポートするのではなく、[参照構文](https://pact-language.readthedocs.io/en/latest/pact-reference.html#references)を使用しましょう。 他のモジュール関数を参照するモジュールを定義する場合は、 モジュール定義時にそれらの参照がインライン化されるため、 `use` を使用しても問題ありません。

3.  ハードコードされた引数とメッセージ値

    トランザクションは、次のように値をトランザクション コードに直接エンコードできます。

    ```lisp
    (accounts.transfer "Acct1" "Acct2" 100.00)
    ```

    または次のように、メッセージ JSON ペイロードから値を読み取ることもできます。

    ```lisp
    (defun transfer-msg ()
      (transfer (read-msg "from") (read-msg "to")
                (read-decimal "amount")))

    ...

    (accounts.transfer-msg)
    ```

    後者では、トランザクション時に解釈されるコードが少ないため、実行速度が少し早くなります。

4.  必要に応じた型指定

    テーブルスキーマによって、Pact はほとんどのユース ケースで厳密に型指定されますが、データベースを使用しない関数でも型指定が必要になることがあります。この場合、REPLの [typecheck](https://pact-language.readthedocs.io/en/latest/typecheck) 関数を使用して、必要な型を追加してください。ランタイムの型施行のコストはわずかです。また型シグネチャが多すぎるとコードが読みにくくなる可能性がありますが、もっとも、型は API の文書化に便利な場合があるため、最後は個別の判断となります。

### 制御フロー<a id="sec-2-5-5"></a>

Pact は [if](https://pact-language.readthedocs.io/en/latest/pact-reference.html#if) 文、制限付きルーピング、そしてもちろん関数の適用をサポートしています。

1.  "If" に要注意

    可能な限り、if を避けてください。分岐が多いほど、コードの理解が困難になり、バグが生じやすくなります。ベスト プラクティスとして、フロントエンドに "処理の内容" を表すコードを指定し、スマート コントラクトには "達成しようとするトランザクションを検証" するコードを配置してください。

    Pact の元の設計では、if (およびループ) がすべて排除されていましたが、このバージョンではユーザーが十分に考慮して機能を使用できるように追加されました。

2.  enforce の使用

    "If" はビジネス ロジックの不変条件を施行する目的では絶対に使用せず、その場合は代わりに [enforce](https://pact-language.readthedocs.io/en/latest/pact-reference.html#enforce) を使用してください。これによってトランザクションが失敗します。

    実際、Pact で許容される "非ローカル終了" は失敗のみです。Pact では "全域性" が重視されるためです。

    Pact 2.3 で追加された [enforce-one](https://pact-language.readthedocs.io/en/latest/pact-reference.html#enforce-one) を使用すると、"式が 1 つでも合格すれば、すべての式が合格となる" という基準に従って施行リストをテストできます。これは、Pact における "例外処理" の唯一の例です。施行が失敗した場合は単純に次のテストが実行され、成功した場合は短絡評価が行われます。

3.  組み込みのキーセットの使用

    組み込みのキーセット関数である [keys-all](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-all)、[keys-any](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-any)、[keys-2](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keys-2) は、インタープリターにハードコードされており、素早く実行できます。カスタムのキーセットはランタイムの解決が必要であり、処理に時間がかかります。

### 関数型言語の概念<a id="sec-2-5-6"></a>

Pact には、関数型プログラミングで最もよく使われる [map](https://pact-language.readthedocs.io/en/latest/pact-reference.html#map)、[fold](https://pact-language.readthedocs.io/en/latest/pact-reference.html#fold)、および [filter](https://pact-language.readthedocs.io/en/latest/pact-reference.html#filter) が搭載されています。これらはすべて、[部分適用](https://pact-language.readthedocs.io/en/latest/pact-reference.html#partial-application)が使用され、リスト項目が引数の末尾に追加されて、関数が順次実行されます。

```lisp
(map (+ 2) [1 2 3])

(fold (+) "" ["Concatenate" " " "me"])
```

Pact にはまた [compose](https://pact-language.readthedocs.io/en/latest/pact-reference.html#compose) の関数が用意されており、他の関数型言語と同様に複数の関数適用を許可します。

### 副作用のない実行<a id="sec-2-5-7"></a>

特定の場合に限り、Pact コードの実行に副作用がまったくないと保証できます。これは、単純にデータベース状態のアクセスや変更が発生しないことを意味します。 現在、 `enforce` 、 `enforce-one` 、およびキーセット述語の評価は、すべて副作用なしの環境で実行されます。 [defconst](https://pact-language.readthedocs.io/en/latest/pact-reference.html) メモ化もそうです。

### LISP<a id="sec-2-5-8"></a>

Pact ではコードがそのランタイム表現を直接反映し、コントラクト作成者がプログラム実行に専念できるように LISP 構文が使用されています。Pact コードは、コードを直接検証できるように、人間が判読可能な形式で台帳上に格納されますが、LISP 型の [S 式構文](https://pact-language.readthedocs.io/en/latest/pact-reference.html#sexp)を使用することで、このコードを高速に実行できます。

### メッセージ データ<a id="sec-2-5-9"></a>

Pact は、JSON ペイロードと署名が付いたメッセージの形式でコードを受け取ります。メッセージ データは [read-msg](https://pact-language.readthedocs.io/en/latest/pact-reference.html#read-msg) と関連の関数を使用して読み取られますが、署名は直接読み書きできず、[キーセット](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysetpredicates)[述語](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysetpredicates)の施行の一部として評価されます。

1.  JSON のサポート

    Pact トランザクションから返される値は、JSON 値として直接表現される必要があります。

    Pact は、[read-msg](https://pact-language.readthedocs.io/en/latest/pact-reference.html#read-msg) 経由でメッセージから JSON 値を読み取るとき、次のようにタイプが自動的に変更されます。

    -   String -> String
    -   Number -> Integer (端数処理されたもの)
    -   Boolean -> Boolean
    -   Object -> Object
    -   Array -> List
    -   Null -> JSON 値

    小数値は文字列として表現され、[read-decimal](https://pact-language.readthedocs.io/en/latest/pact-reference.html#read-decimal) で読み取られます。

## 機密保持<a id="sec-2-6"></a>

Pact は、参加者の一部のみがメッセージを表示できる "機密保持" 環境での使用を想定して設計されています。これは、スマート コントラクト実行に大きな影響があります。

### エンティティ<a id="sec-2-6-1"></a>

"エンティティ" とは、機密メッセージを表示できるまたは表示できないビジネス参加者です。エンティティは会社、会社内のグループ、または個人のいずれかです。

### 互いに素なデータベース<a id="sec-2-6-2"></a>

Pact スマート コントラクトは、ブロックチェーンによって編成されたメッセージを処理して、トランザクション実行の結果をデータベースに記録します。 機密保持環境では、異なるエンティティは異なるトランザクションを実行します。このため、データベースが「互いに素」となります。

このことは、Pact の実行には影響がありませんが、データベースのデータが "両面トランザクション" を行えなくなるため、互いに素になった複数のデータセットに対して単一トランザクションを行う新しい概念が必要になります。

### 機密保持のための pacts<a id="sec-2-6-3"></a>

Pact の重要な機密保持機能は、素である複数のトランザクションを調整し、ターゲットのエンティティによる実行の順番を調整する機能です。 これについては、次のセクションで説明します。

## "Pacts" による非同期トランザクションの自動化<a id="sec-2-7"></a>

プログラミング言語名の Pact とは名前が似ていますが、“pacts” は別のものです。本来は「約束」や「契約」のような意味で、ここでは多段階の順次トランザクションを [pact](https://pact-language.readthedocs.io/en/latest/pact-reference.html#defpact) と呼ばれる単一のコードとして定義したものです。複数段階のインタラクションを 1 つの pact として定義することにより、トランザクションの参加者は、合意された一連の操作を実行し、特定の複数段階のインタラクションが存続する間のみ、データ リソースの作成と管理に使用できる、特別な "実行スコープ" を提供します。

pacts は、"コルーチン" の一種、つまり、複数の終了ポイントと再エントリ ポイントを持つ関数です。Pacts は、[ステップ](https://pact-language.readthedocs.io/en/latest/pact-reference.html#step)から成り、特定のブロックチェーン トランザクションでは単一のステップのみが実行されます。ステップは、厳格な順番に従ってのみ実行されます。

1 つの pact は、関数の定義と同様、引数を宣言して定義されます。ただし、引数の値は最初のステップの実行時にのみ評価され、その後、 それらの値は変更されることなく後続のステップでも使用できます。新しい値を後続のステップと共有するには、ステップで [yield](https://pact-language.readthedocs.io/en/latest/pact-reference.html#yield) を使用して値を引き渡します。後続のステップは、 [resume](https://pact-language.readthedocs.io/en/latest/pact-reference.html#resume) を使用してこの値を拾うことができます。

pacts は、プライベートとパブリックの2つのコンテキストのいずれかで実行するように設計されています。プライベート pact では、そのステップを実行する単一のエンティティが各ステップの指定によって特定されているのに対し、パブリック ステップではエンティティの指定がありません。 pacts は、パブリックまたはプライベートのいずれかでのみ実行できます。エンティティの指定があるステップとないステップが混在していると、ロード時にエラーが発生します。

### パブリック pacts<a id="sec-2-7-1"></a>

パブリック pacts は、厳格な順番でのみ実行できるステップで構成されています。 どのユーザーがステップを実行できるかの施行は、ステップ式のコード内で行われます。 すべてのステップは、トランザクションの参加者が、ブロックチェーンに CONTINUATION コマンドを送信してマニュアルで開始されます。

### プライベート pacts<a id="sec-2-7-2"></a>

プライベート pacts も順次実行されるステップで構成されますが、各ステップは "entity" 引数で選択されたエンティティ ノードでのみ実行でき、他のエンティティ ノードはステップを無視 します。プライべート pacts は、初期ステップが送信された後、ブロックチェーン プラットフォームによって自動的に続行されます。実行エンティティのノードは、自動的に CONTINUATION コマンドを送信して次のステップを促します。

### 失敗、ロールバック、キャンセル<a id="sec-2-7-3"></a>

失敗処理は、パブリック pacts とプライベート pacts で大きく異なります。

パブリック pacts では、このステップで pact を 中止 できるかを示すロールバック式が指定されます。中止できる場合、参加者は次のステップが実行される前に CANCEL メッセージを送信してキャンセルを実行できます。pact の最後のステップが実行された後は、pact が終了し、ロール バックすることはできません。パブリック ステップでの失敗は、pact 以外のトランザクションでの失敗と同様、すべての変更がロール バックされます。したがって pacts は意図的にしか中止できないため、必要になりうるすべての中止選択を前もって用意しましょう。

プライベート pact では、ステップの順次実行が、ブロックチェーン プラットフォーム自体によって自動に行われます。失敗が発生すると ROLLBACK メッセージが実行エンティティ ノードから送信されます。これにより、前のステップで指定されたロールバック式が発動され、そのステップのエンティティによって実行されます。この失敗は次に、新しい ROLLBACK トランザクションとして前のステップに戻り、最初のステップがロール バックを終えたときに完了します。

### イールドと再開<a id="sec-2-7-4"></a>

ステップは、[yield](https://pact-language.readthedocs.io/en/latest/pact-reference.html#yield) と [resume](https://pact-language.readthedocs.io/en/latest/pact-reference.html#resume) によって、次のステップに値を渡すことができます。パブリックの場合、この値はブロックチェーンの pact 範囲内で維持されるため改ざんできません。プライベートの場合、これは単に、実行されたエンティティから RESUME メッセージと共に送信される値です。

### Pact 実行スコープと `pact-id`<a id="sec-2-7-5"></a>

pact は開始されるたびに、特有の ID が付けられます。[pact-id](https://pact-language.readthedocs.io/en/latest/pact-reference.html#pact-id) 関数は現在実行されている pact の ID を譲るか、pact 範囲内で実行されていない場合は失敗します。したがって、キーセットと署名を使用するのと同じように、この仕組みを使用してリソースのアクセスを保護することができます。使い方の例としては、特定の pact のコンテキスト内でのみ使用できるエスクロー (第三者預託) アカウントを作成すれば、第三者を置く必要が多くの場合なくなります。

### pacts のテスト<a id="sec-2-7-6"></a>

pacts をテストするには、repl 関数 [env-entity](https://pact-language.readthedocs.io/en/latest/pact-reference.html)、[env-step](https://pact-language.readthedocs.io/en/latest/pact-reference.html)、[pact-state](https://pact-language.readthedocs.io/en/latest/pact-reference.html#pact-state) を使用して pactの仮実行を行います。

また pact サーバー API で pact 実行をシミュレートすることもできます。これには、[継続リクエスト](https://pact-language.readthedocs.io/en/latest/pact-reference.html#request-yaml) yaml ファイルを `cont` ペイロード付きの API リクエストにフォーマットします。

## モジュールの依存管理<a id="sec-2-8"></a>

Pact は、モジュールと他の Pact モジュールとの依存関係を管理するための複数の機能をサポートしています。

### モジュール ハッシュ<a id="sec-2-8-1"></a>

ロードされた pact モジュールは、モジュールのソース コード内容に基づいて計算されたハッシュに関連付けられます。このモジュール ハッシュは、モジュールのバージョンを一意に識別します。モジュール ハッシュは、[describe-module](https://pact-language.readthedocs.io/en/latest/pact-reference.html#describe-module) を使用して次のように確認できます。

```
pact> (at "hash" (describe-module 'accounts))
"9d6f4d3acb2fd528206330d09a8926da6abdd9ac5e8c4b24cc35955203f234688c25f9545ead56f783c5269fe4be6a62aa89162caf811142572ac172dc2adb91"
```

### `use` を使用したモジュール バージョンの固定<a id="sec-2-8-2"></a>

[use](https://pact-language.readthedocs.io/en/latest/pact-reference.html#use) という関数を使用すると、モジュール ハッシュを指定して、依存関係のバージョンを固定できます。モジュール宣言内で使用すると、依存関係ハッシュ値がモジュールのハッシュに導入されます。これにより、"依存関係のみ" のアップグレードで、アップグレードをモジュール バージョンにプッシュできます。

### インライン化された依存のモジュール<a id="sec-2-8-3"></a>

Pact では、モジュールがロードされると、すべてのユーザーコード参照がインライン化されます。 つまり、アップストリームのコードがダウンストリームのモジュールに直接挿入されます。 ここでインラインされた他所のコードは、もう変えられません。モジュール コードを再ロードしない限り、依存のモジュールをアップグレードすることはできません。

これはユーザーにとっても安心な仕様だと言えます。自らのモジュールがロードされれば、 アップストリームからの干渉はありません。しかしこれは、そのアップストリームの開発者にとって大問題でもあります。 バッグを解決したいときや新しい機能を導入したい場合は、コードの古いバージョンが 既にユーザーモジュールにインラインされたため、変更を加えられなくなります。この問題の解決は次の部分で説明されます。

### ハッシュの「ブレス」<a id="sec-2-8-4"></a>

上で説明した問題のバランスを取る方法があります。Pact では、アップストリームのモジュールがそれに依存を持つダウンストリームのコードを無効にすることができます。アップストリームの開発者が [bless](https://pact-language.readthedocs.io/en/latest/pact-reference.html#bless) （ブレス）という特殊形式をモジュールに入れれば、特定の古いバージョンにのみテーブルへのアクセスが許可されます。

```lisp
(module provider 'keyset
  (bless "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94")
  (bless "ca002330e69d3e6b84a46a56a6533fd79d51d97a3bb7cad6c2ff43b354185d6dc1e723fb3db4ae0737e120378424c714bb982d9dc5bbd7a0ab318240ddd18f8d")
  ...
)
```

これを “provider” というモジュールの最新の形とし、指定されたハッシュは “provider” の古いバージョンに一致します。このハッシュ以外のバージョンがデータベースを更新しようとする際、失敗します。 幸い、副作用（データベースの更新など）のないコードは、ブレスはどうであれ、無効になることは決してありません。 これはアップストリームからの妨害を防止します。

### "v2" モジュールによる段階的アップグレード<a id="sec-2-8-5"></a>

アップストリームの開発者は、ブレスの仕組みを使用して、重要なアップグレードを段階的に実施できます。 これには、アップグレード前のモジュールの名前を新しいバージョンの名前に変更し、古いモジュールを、最新バージョン (およびそれ以前の適切なバージョン) のみをブレスする新しい “空の” モジュールに置き換えます。 新しいユーザーは "v1" コードのインポートに失敗し、新しいバージョンを使用する必要がありますが、 既存のユーザーは、期限まで古いバージョンを引き続き使用できます。 期限とは、モジュールの開発者がすべてのブレスを削除していいと判断する期間のことです。そして、"空の" モジュールは、ユーザーデータを新しいモジュールに移行するための移行機能も提供できます。

# シンタックス<a id="sec-3"></a>

## リテラル<a id="sec-3-1"></a>

### 文字列<a id="sec-3-1-1"></a>

文字列リテラルは、二重引用符によって作成します。

```
pact> "a string"
"a string"
```

文字列はまた、空白の前後にバックスラッシュを挿入することで、複数行にわたって記述することができます (REPL ではできません)。

```lisp
(defun id (a)
 "Identity function. \
 \Argument is returned."
 a)
```

### シンボル<a id="sec-3-1-2"></a>

シンボルとは、関数やテーブル名など、ランタイム内の一意の項目を表す文字列リテラルです。シンボル表現は内部的には単なる文字列リテラルであり、慣用に従って使用できます。

シンボルは、引用符を前に付けて作成するため、空白や複数行の記述はサポートされていません。

```
pact> 'a-symbol
"a-symbol"
```

### 整数<a id="sec-3-1-3"></a>

整数リテラルは、大きさに限界のない自然数です。

```
pact> 12345
12345

pact> -922337203685477580712387461234
-922337203685477580712387461234
```

### 小数<a id="sec-3-1-4"></a>

小数リテラルは、精度に限界のない浮動小数です。

```
pact> 100.25
100.25

pact> -356452.234518728287461023856582382983746
-356452.234518728287461023856582382983746
```

### ブール<a id="sec-3-1-5"></a>

ブールは、 `true` リテラルと `false` リテラルで表されます。

```
pact> (and true false)
false
```

### リスト<a id="sec-3-1-6"></a>

リストリテラルは、角かっこを使って作成します。必要に応じて、カンマで区切ることができます。均一なリテラル リストは、解析時に型が与えられます。

```
pact> [1 2 3]
[1 2 3]

pact> [1,2,3]
[1 2 3]

pact> (typeof [1 2 3])
"[integer]"

pact> (typeof [1 2 true])
"list"
```

### オブジェクト<a id="sec-3-1-7"></a>

オブジェクトは Python や Javascript でも見るディクショナリであり、中かっこの中に、キーと値のペアをコロン (:) で区切って指定します。アプリケーションによっては (例えばデータベースの更新など)、キーを文字列にする必要があります。

```
pact> { "foo": (+ 1 2), "bar": "baz" }
(TObject [("foo",3),("bar","baz")])
```

### バインディング<a id="sec-3-1-8"></a>

バインディングは、オブジェクトと同様に中かっこで作成され、 `:=` 演算子を使用してデータベースの結果を変数にバインドします。これらは、[with-read](https://pact-language.readthedocs.io/en/latest/pact-reference.html#with-read)、[with-default-read](https://pact-language.readthedocs.io/en/latest/pact-reference.html#with-default-read)、[bind](https://pact-language.readthedocs.io/en/latest/pact-reference.html#bind)、[resume](https://pact-language.readthedocs.io/en/latest/pact-reference.html#resume) で使用して、行内の名前付き列、またはオブジェクトの値に変数を割り当てます。

```lisp
(defun check-balance (id)
  (with-read accounts id { "balance" := bal }
    (enforce (> bal 0) (format "Account in overdraft: {}" [bal]))))
```

## 型の指定<a id="sec-3-2"></a>

型は、コロン (`:`) 演算子の後に、型リテラルまたはユーザーの定義した型で指定できます。

### 型リテラル<a id="sec-3-2-1"></a>

-   string
-   integer
-   decimal
-   bool
-   time
-   keyset
-   list、または [type] (リスト型を指定)
-   object (スキーマを使用してさらに型指定が可能)
-   table (スキーマを使用してさらに型指定が可能)
-   value (JSON 値)

### スキーマ型リテラル<a id="sec-3-2-2"></a>

[defschema](https://pact-language.readthedocs.io/en/latest/pact-reference.html) で定義されたスキーマは、中かっこで囲んだ名前によって参照されます。

```
table:{accounts}
object:{person}
```

### 型指定の対象<a id="sec-3-2-3"></a>

1.  関数の引数と戻り値の型

    ```lisp
    (defun prefix:string (pfx:string str:string) (+ pfx str))
    ```

2.  let 変数

    ```lisp
    (let ((a:integer 1) (b:integer 2)) (+ a b))
    ```

3.  テーブルとオブジェクト

    テーブルとオブジェクトは、スキーマ型リテラルのみを指定できます。

    ```lisp
    (deftable accounts:{account})

    (defun get-order:{order} (id) (read orders id))
    ```

4.  定数

    ```lisp
    (defconst PENNY:decimal 0.1)
    ```

## 特殊形式<a id="sec-3-3"></a>

### ドキュメントとメタデータ<a id="sec-3-3-1"></a>

[defun](https://pact-language.readthedocs.io/en/latest/pact-reference.html) などの多くの特殊形式では、必要に応じて次の形式でドキュメント文字列を指定できます。

```lisp
(defun average (a b)
  "take the average of a and b"
  (/ (+ a b) 2))
```

ただしここでは、ドキュメント文字列以外のメタデータも指定できます。”@model” を加えれば、コードの正確性を自動的に確認してくれる `property` を定義できます。

```lisp
(defun average (a b)
  @doc "take the average of a and b"
  @model (property (= (+ a b) (* 2 result)))
  (/ (+ a b) 2))
```

やはり、 `foo` も `@doc foo` の略した形です。 将来的には、使用できるメタデータの種類が追加される予定です。

Properties の詳細は[こちら](https://pact-language.readthedocs.io/en/latest/pact-properties.html)へ。

### bless<a id="sec-3-3-2"></a>

```lisp
(bless HASH)
```

モジュール宣言内で、HASH によってそのモジュールの前のバージョンを ”ブレス” し、データベースのアクセス許可を設定します。この仕組みについては、「[モジュールの依存管理](https://pact-language.readthedocs.io/en/latest/pact-reference.html#dependency-management)」を参照してください。

```lisp
(module provider 'keyset
  (bless "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94")
  (bless "ca002330e69d3e6b84a46a56a6533fd79d51d97a3bb7cad6c2ff43b354185d6dc1e723fb3db4ae0737e120378424c714bb982d9dc5bbd7a0ab318240ddd18f8d")
  ...
)
```

### defun<a id="sec-3-3-3"></a>

```lisp
(defun NAME ARGLIST [DOC-OR-META] BODY...)
```

NAME を関数として定義し、ARGLIST を引数として指定し、オプションで DOC-OR-META を指定します。引数は、1 つ以上の式である BODY のスコープで使えます。

```lisp
(defun add3 (a b c) (+ a (+ b c)))

(defun scale3 (a b c s)
  "multiply sum of A B C times s"
  (* s (add3 a b c)))
```

### defconst<a id="sec-3-3-4"></a>

```lisp
(defconst NAME VALUE [DOC-OR-META])
```

NAME を VALUE として定義し、オプションで DOC-OR-META を指定します。値はモジュールのロード時に評価され記録されます。

```lisp
(defconst COLOR_RED="#FF0000" "Red in hex")

(defconst COLOR_GRN="#00FF00" "Green in hex")

(defconst PI 3.14159265 "Pi to 8 decimals")
```

### defpact<a id="sec-3-3-5"></a>

```lisp
(defpact NAME ARGLIST [DOC-OR-META] STEPS...)
```

NAME を、"pact"、つまり複数ステップ計算として定義します。[defun](https://pact-language.readthedocs.io/en/latest/pact-reference.html) とほぼ同じですが、厳格な順序で実行される[ステップ](https://pact-language.readthedocs.io/en/latest/pact-reference.html#step)で本体を構成する必要があります。ステップは、"パブリック" (エンティティ指定なし)、または "プライベート" (エンティティ指定あり) のいずれかで統一する必要があります。プライベート ステップで失敗が生じた場合、逆の順序で "ロールバック カスケード" が発生します。

```lisp
(defpact payment (payer payer-entity payee payee-entity amount)
  (step-with-rollback payer-entity
    (debit payer amount)
    (credit payer amount))
  (step payee-entity
    (credit payee amount)))
```

### defschema<a id="sec-3-3-6"></a>

```lisp
(defschema NAME [DOC-OR-META] FIELDS...)
```

NAME を、FIELDS のリストを指定する "スキーマ" として定義します。各フィールドは、 `FIELDNAME[:FIELDTYPE]` の形式です。

```lisp
(defschema accounts
  "Schema for accounts table".
  balance:decimal
  amount:decimal
  ccy:string
  data)
```

### deftable<a id="sec-3-3-7"></a>

```lisp
(deftable NAME[:SCHEMA] [DOC-OR-META])
```

NAME を、データベース関数で使用される "テーブル" として定義します。テーブルは、[create-table](https://pact-language.readthedocs.io/en/latest/pact-reference.html#create-table) で作成する必要があることに注意してください。

### let<a id="sec-3-3-8"></a>

```lisp
(let (BINDPAIR [BINDPAIR [...]]) BODY)
```

BINDPAIR 内の変数を BODY のスコープ内にバインドします。BINDPAIR 内の変数は、同じ `let` バインディング内の以前に宣言された変数を参照することはできません。その場合は、[let\*](https://pact-language.readthedocs.io/en/latest/pact-reference.html#letstar) を使用してください。

```lisp
(let ((x 2)
      (y 5))
  (* x y))
> 10
```

### let\*<a id="sec-3-3-9"></a>

```lisp
(let* (BINDPAIR [BINDPAIR [...]]) BODY)
```

BINDPAIR 内の変数を BODY の範囲内にバインドします。変数は、同じ `let` で以前に宣言された BINDPAIRS を参照できます。 `let*` は、コンパイル時に、各 BINDPAIR のネストされた `let` 呼び出しに拡張されます。そのため、可能な限り、let の使用が推奨されます。

```lisp
(let* ((x 2)
       (y (* x 10)))
  (+ x y))
> 22
```

### step<a id="sec-3-3-10"></a>

```lisp
(step EXPR)
(step ENTITY EXPR)
```

[defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 内のステップは、前のステップが前のトランザクションで実行され、後のステップが後のトランザクションで実行されるように定義します。ENTITY を指定すると、このステップはプライベートトランザクションとなり、ENTITY のみがこのステップを実行して、他の参加者はこのステップを無視します。

### step-with-rollback<a id="sec-3-3-11"></a>

```lisp
(step-with-rollback EXPR ROLLBACK-EXPR)
(step-with-rollback ENTITY EXPR ROLLBACK-EXPR)
```

[defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 内でステップを定義します。[step](https://pact-language.readthedocs.io/en/latest/pact-reference.html#step) に似ていますが、ROLLBACK-EXPR を指定する点が異なります。ENTITY を指定すると、ROLLBACK-EXPR は後続のステップが失敗した場合のみ、失敗したステップから最初のステップまで逆方向に実行される "ロールバック カスケード" の一部として実行されます。ENTITY を指定しない場合、ROLLBACK-EXPR 関数は、参加者が意図的に実行する "キャンセル関数" として機能します。

### use<a id="sec-3-3-12"></a>

```lisp
(use MODULE)
(use MODULE HASH)
```

既存の MODULE を名前空間にインポートします。コードファイルの一番上、またはモジュール宣言内でのみ使えます。MODULE は、文字列、シンボル、アトムのいずれかです。 HASH を指定すると、モジュール ハッシュが HASH と一致しているかどうかが検証され、そうでない場合は失敗します。ブロックチェーン上にロードされたモジュールのハッシュを照会するには、[describe-module](https://pact-language.readthedocs.io/en/latest/pact-reference.html#describe-module) を使用します。

```lisp
(use accounts)
(transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
"Write succeeded"
```

### module<a id="sec-3-3-13"></a>

```lisp
(module NAME KEYSET [DOC-OR-META] DEFS...)
```

キーセット KEYSET によってガードされたモジュール NAME を定義してブロックチェーンにインストールします。オプションで DOC-OR-META を指定できます。DEFS には、[defun](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 式または [defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 式のみを指定できます。

```lisp
(module accounts 'accounts-admin
  "Module for interacting with accounts"

  (defun create-account (id bal)
    "Create account ID with initial balance BAL"
    (insert accounts id { "balance": bal }))

  (defun transfer (from to amount)
    "Transfer AMOUNT from FROM to TO"
    (with-read accounts from { "balance": fbal }
      (enforce (<= amount fbal) "Insufficient funds")
      (with-read accounts to { "balance": tbal }
        (update accounts from { "balance": (- fbal amount) })
        (update accounts to { "balance": (+ tbal amount) }))))
  )
```

## 式<a id="sec-3-4"></a>

式には[リテラル](https://pact-language.readthedocs.io/en/latest/pact-reference.html#literals)、アトム、S 式、参照のいずれかを使用できます。

### アトム<a id="sec-3-4-1"></a>

アトムは先頭に文字または許可されたシンボルを使用し、文字、数字および許可されたシンボルで構成される非予約語です。許可されたシンボルは、%#+-\\\_&$@<>=?\*!|/ です。アトムは、[defun](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 形式、[defpact](https://pact-language.readthedocs.io/en/latest/pact-reference.html) 形式、[binding](https://pact-language.readthedocs.io/en/latest/pact-reference.html#bindings) 形式によってバインドされる変数、 または [use](https://pact-language.readthedocs.io/en/latest/pact-reference.html#use) によって名前空間にインポートされたシンボルに解決する必要があります。

### S 式<a id="sec-3-4-2"></a>

S 式はかっこで囲って作成されます。最初のアトムは、式が[特殊形式](https://pact-language.readthedocs.io/en/latest/pact-reference.html#special)か関数適用かを示します。関数適用の場合、最初のアトムは定義を参照する必要があります。

1.  部分適用

    適用する引数が必要な数よりも少なくても、コンテキストによってはその関数の有効な/部分適用/となります。ただし、これがサポートされるのは Pact の[関数型関数](https://pact-language.readthedocs.io/en/latest/pact-reference.html#functional-concepts)のみであり、それ以外は、ランタイム エラーとなります。

### 参照<a id="sec-3-4-3"></a>

参照は、ピリオド (`.`) によって結合された2つ以上のアトムであり、モジュール定義に直接解決されます。

```
pact> accounts.transfer
"(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT
from SRC to DEST\")"

pact> transfer
Eval failure:
transfer<EOF>: Cannot resolve transfer

pact> (use 'accounts)
"Using \"accounts\""

pact> transfer
"(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT
from SRC to DEST\")"
```

参照は `use` よりも高速に解決できるため、トランザクションでは参照の方が適切です。 しかし、モジュール定義では、 `use` の方が読みやすくなります。

# 時間形式<a id="sec-4"></a>

Pact は、Haskell の [thyme ライブラリ](http://hackage.haskell.org/package/thyme) を利用して、時間値の計算を高速に処理します。 以下に示すように、[parse-time](https://pact-language.readthedocs.io/en/latest/pact-reference.html#parse-time) 関数と [format-time](https://pact-language.readthedocs.io/en/latest/pact-reference.html#format-time) 関数では、GNU strftime から派生した形式コードを指定し、一部の拡張機能を利用できます。

%% - リテラル "%"

%z - RFC 822/ISO 8601:1988 型の数値タイム ゾーン (例: "-0600"、"+0100")

%N - ISO 8601 型の数値タイム ゾーン (例: "-06:00"、"+01:00") *拡張機能*

%Z - タイムゾーン名

%c - 現在のロケール用のカレンダー時間の優先的表現。‘dateTimeFmt'locale (例: %a %b %e %H:%M:%S %Z %Y) の形式で指定

%R - %H:%M と同じ

%T - %H:%M:%S と同じ

%X - 現在のロケール用の時刻の優先的表現。‘timeFmt' locale (例: %H:%M:%S) の形式で指定

%r - 現在のロケール用の午前/午後形式を使用した完全なカレンダー時間。‘time12Fmt' locale (例: %I:%M:%S %p) の形式で指定

%P - 午前/午後の表記 (‘amPm' locale)、小文字に変換、"am"、"pm"

%p - 午前/午後の表記 (‘amPm' locale)、"AM"、"PM"

%H - 時刻 (24 時間制)、2 文字になるまで先頭をゼロ埋め、"00" ～ "23"

%k - 時刻 (24 時間制)、2 文字になるまで先頭を空白埋め、" 0" ～ "23"

%I - 午前/午後の時刻 (12 時間制)、2 文字になるまで先頭をゼロ埋め、"01" ～ "12"

%l - 午前/午後の時刻 (12 時間制)、2 文字になるまで先頭を空白埋め、" 1" ～ "12"

%M - 分、2 文字になるまで先頭をゼロ埋め、"00" ～ "59"

%S - 秒 (小数部分なし)、2 文字になるまで先頭をゼロ埋め、"00" ～ "60"

%v - マイクロ秒、6 文字になるまで先頭をゼロ埋め、"000000" ～ "999999"。/拡張機能/

%Q - 小数点と小数秒、小数点以下 6 桁まで、末尾のゼロを省く。整数秒については、%Q によって空の文字列を生成。/拡張機能/

%s - UNIX エポックからの整数秒。UNIX エポックより前の時刻は、負の数となる。%s.%q と %s%Q では、小数部分が正の数であって負の数でないことに注意してください。例えば、UNIX エポック前 0.9 秒は、%s%Q で "-1.1" と形式化します。

%D - %m\\/%d\\/%y と同じ

%F - %Y-%m-%d と同じ

%x - ‘dateFmt' locale (例: %m\\/%d\\/%y) の形式で指定

%Y - 年、文字埋めなし。

%y - 西暦の下 2 桁、2 文字になるまで先頭をゼロ埋め、"00" ～ "99"

%C - 世紀、文字埋めなし。

%B - 月の名前、長い形式 (‘months' の 'fst' locale)、"January" ～ "December"

%b, %h - 月の名前、短い形式 (‘months' の ‘snd' locale)、"Jan" ～ "Dec"

%m - 月番号、2 文字になるまで先頭をゼロ埋め、"01" ～ "12"

%d - 日、2 文字になるまで先頭をゼロ埋め、"01" ～ "31"

%e - 日、2 文字になるまで先頭を空白埋め、" 1" ～ "31"

%j - 元旦から数えた日数、3 文字になるまで先頭をゼロ埋め、"001" ～ "366"

%G - 週/日形式の年、文字埋めなし。

%g - 週/日形式の西暦の下 2 桁、2 文字になるまで先頭をゼロ埋め、"00" ～ "99"

%f - 週/日形式の世紀、文字埋めなし。/拡張機能/

%V - 週/日形式の週番号、2 文字になるまで先頭をゼロ埋め、"01" ～ "53"

%u - 週/日形式の曜日、"1" ～ "7"

%a - 曜日、短い形式 (‘wDays' の ‘snd'locale)、"Sun" ～ "Sat"

%A - 曜日、長い形式 (‘wDays' の ‘fst'locale)、"Sunday" ～ "Saturday"

%U - 日曜日始まりの週番号 (‘sundayStartWeek')、2 文字になるまで先頭をゼロ埋め、"00" ～ "53"

%w - 曜日番号、"0" (= 日曜日) ～ "6" (= 土曜日)

%W - 月曜日始まりの週番号 (‘Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek')、2 文字になるまで先頭をゼロ埋め、"00" ～ "53"

注: %q (ピコ秒、先頭をゼロ埋め) は正しく機能しないため、本書に記載されていません。

## デフォルト形式と JSON のシリアル化<a id="sec-4-1"></a>

デフォルト形式は、UTC ISO8601 の日付 + 時刻形式、“%Y-%m-%dT%H:%M:%SZ” であり、この形式で [time](https://pact-language.readthedocs.io/en/latest/pact-functions.html#id4) 関数に入力できます。time オブジェクトは、内部的にはマイクロ秒までの解像度をサポートしますが、Pact インタープリターから JSON として返される値は、デフォルト形式でシリアル化されます。より高い解像度が必要な場合は、 `%v` とその関連項目を使用して、明示的に時間形式を設定してください。

## 例<a id="sec-4-2"></a>

### ISO8601<a id="sec-4-2-1"></a>

```
pact> (format-time "%Y-%m-/%d/T%H:%M:%S%N" (time "2016-07-23T13:30:45Z"))
"2016-07-23T13:30:45+00:00"
```

### RFC822<a id="sec-4-2-2"></a>

```
pact> (format-time "%a, %\_d %b %Y %H:%M:%S %Z" (time "2016-07-23T13:30:45Z"))
"Sat, 23 Jul 2016 13:30:45 UTC"
```

### YYYY-MM-DD hh:mm:ss.000000<a id="sec-4-2-3"></a>

```
pact> (format-time "%Y-%m-/%d/ %H:%M:%S.%v" (add-time (time "2016-07-23T13:30:45Z") 0.001002))
"2016-07-23 13:30:45.001002"
```

# データベースのシリアル化形式<a id="sec-5"></a>

## 試行的なベータ版の機能に関する重要な警告<a id="sec-5-1"></a>

このセクションでは、Pact 2.4.\* 以降のバージョンに搭載されているデータベース シリアル化形式について記載されています。しかし、この形式はまだベータ版です。これは、当社がこのデータを直接エクスポートする具体的な RDBMS バックエンドと展開について取り組みを開始してからまだ日が浅いためです。

したがって、当社はこれらの形式について後方互換性を保証せず、将来のバージョンにおいて改良された形式に移行する権利を留保します。Pact では API の安定性については、お客様にとっての互換性とパフォーマンスが最優先事項ですが、バックエンドのエクスポートはまだ試行的な機能です。

将来的にはこれらの形式について安定性が確保できると考えており、そのときには後方互換性が保証されます。

## JSON 値によるキー値の形式<a id="sec-5-2"></a>

Pact はすべての値を 2 列、2 値の構造でデータベースに格納し、すべての値は JSON で表現されます。このアプローチは、JSONの汎用性に重点を置いて選択されたものです。

透過性: JSON は、人間が判別可能な形式であり、値を目視で確認できます。

移植性: 本書執筆の時点 (2018 年) で、JSON はほぼすべてのデータベース バックエンドにサポートされています。キー/値構造により、RDBMS 以外の RocksDB などのバックエンドも使用でき、また SQL DDL を単純な主要キー構造で容易に使用できます。インデックス処理はサポートされておらず、不要です。

## Pact データ型のコーデック<a id="sec-5-3"></a>

サポートされているすべての Pact データ型は、シリアル化の速度と正確性のために設計された特殊なコーデックを使用して、フロントエンド API で使用される JSON 形式とは異なる JSON にエンコードされます。

### 整数<a id="sec-5-3-1"></a>

大きな整数を除き、値は直接 JSON の数値にエンコードされます。

JSON/Javascript で、何をもって "大きな整数" とするかは議論があります。当社では `[-2^53 .. 2^53]` という範囲を使用しています (詳細については、[こちら](http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html)を参照してください)。大きな整数については、文字化された整数値を使用して、JSON シングルトン オブジェクトがエンコードされます。

```js
/* small integers are just a number */
1

/* large integers are objects */
{ "_P_int": "123..." /* integer string representation */
}
```

### 小数<a id="sec-5-3-2"></a>

小数は、[Haskell](https://hackage.haskell.org/package/Decimal-0.5.1/docs/Data-Decimal.html#t:DecimalRaw)[小数形式](https://hackage.haskell.org/package/Decimal-0.5.1/docs/Data-Decimal.html#t:DecimalRaw)に従い、"小数位" と "仮数" を使用してエンコードされます。

```js
{ "_P_decp": 4 /* decimal places */
, "_P_decm": 15246 /* decimal mantissa, encoded using INTEGER format */
}
```

仮数の値は、上記の整数形式を使用します。小数に関する資料に記載されているとおり、値は次のように計算できます。

```
MANTISSA / (10 ^ PLACES)
```

### ブール<a id="sec-5-3-3"></a>

ブールは、JSON ブールとして格納されます。

### 文字列<a id="sec-5-3-4"></a>

文字列は、JSON 文字列として格納されます。

### 時刻<a id="sec-5-3-5"></a>

時刻は、修正ユリウス通日の値および日単位のローカルマイクロ秒の値を示す JSON オブジェクトに格納されます。

```js
{ "_P_timed": 234 /* "modified julian day value */
, "_P_timems": 32495874 /* microseconds, encoded using INTEGER format */
}
```

MJD の変換に関する推奨事項は、[こちら](https://stackoverflow.com/questions/11889553/convert-modified-julian-date-to-utc)を参照してください。

### JSON 値/blob<a id="sec-5-3-6"></a>

未処理の JSON blob は、変更なしでコンテナー オブジェクトにエンコードされます。

```js
{ "_P_val": { "foo": "bar" } /* unmodified user JSON object */
}
```

### キーセット<a id="sec-5-3-7"></a>

キーセットは、キー リストと述語名を JSON オブジェクトに格納します。

```js
{ "_P_keys": ["key1","key2"] /* public key string representations */
, "_P_pred": "keys-all" /* predicate function name */
}
```

## モジュール (ユーザー) テーブル<a id="sec-5-4"></a>

Pact コードで指定された各モジュール テーブルに対して、"データ テーブル" と "トランザクション テーブル" の 2 つのバックエンド テーブルが作成されます。

### 列名<a id="sec-5-4-1"></a>

どのテーブルでも単純に、 `t_key` と `t_value` です。

### ユーザー データ テーブル<a id="sec-5-4-2"></a>

データ テーブルは、現在のテーブル状態への CRUD 型のアクセスをサポートします。

-   **名前**: `USER_[module]_[table]`.
-   **キー形式**: キーは、テキスト/VARCHAR であり、サポートされる最大長は、バックエンドによって変わります。
-   **値形式**: JSON オブジェクト (ユーザー指定のキーおよびコーデック変換された値)。

### ユーザー トランザクション テーブル<a id="sec-5-4-3"></a>

トランザクション テーブルは、テーブルへのすべての更新を記録します。

-   **名前**: `TX_[module]_[table]`.
-   **キー形式**: キーは整数です。記録されるトランザクション ID を反映したバックエンド固有の BIGINT 値が使用されます。
-   **値の形式**: 特定のトランザクションでの更新から成る JSON 配列です。

更新形式は、以下の JSON オブジェクトです。

```js
{ "table": "name"  /* user-visible table name (not backend table name) */
, "key": "123"     /* update string key */
, "value": { ... } /* The new JSON row value. Entire row is captured.*/
}
```

JSON の行の値には、ユーザー データ テーブルと同じエンコーディングが使用されます。

# 組み込み関数<a id="sec-6"></a>

## 一般的な関数<a id="sec-6-1"></a>

### at<a id="sec-6-1-1"></a>

*idx* `integer` *list* `[<l>]` *→* `<a>`

*idx* `string` *object* `object:<{o}>` *→* `<a>`

リストの場合 IDX に一致する添字の値を譲ります。オブジェクトの場合 IDX に一致するキーの関連された値を譲ります。

```
pact> (at 1 [1 2 3])
2

pact> (at "bar" { "foo": 1, "bar": 2 })
2
```

### bind<a id="sec-6-1-2"></a>

*src* `object:<{row}>` *binding* `binding:<{row}>` *→* `<a>`

SRC を評価し、BINDING 内でキーの指定によって値を抽出してある名前に”バインド”します。バインドされた名前は後続の式で使えます。

```
pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
1
```

### compose<a id="sec-6-1-3"></a>

*x* `(x:<a> -> <b>)` *y* `(x:<b> -> <c>)` *value* `<a>` *→* `<c>`

X の演算を VALUE に対して行い、Y の演算を X の結果に対して行うように、X と Y を合成します。

```
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```

### constantly<a id="sec-6-1-4"></a>

*value* `<a>` *ignore1* `<b>` *→* `<a>`

*value* `<a>` *ignore1* `<b>` *ignore2* `<c>` *→* `<a>`

*value* `<a>` *ignore1* `<b>` *ignore2* `<c>` *ignore3* `<d>` *→* `<a>`

引数 IGNORE\* を計算せずに無視して VALUE を返します。

```
pact> (filter (constantly true) [1 2 3])
[1 2 3]
```

### contains<a id="sec-6-1-5"></a>

*value* `<a>` *list* `[<a>]` *→* `bool`

*key* `<a>` *object* `object:<{o}>` *→* `bool`

*value* `string` *string* `string` *→* `bool`

LIST または STRING が VALUE を含んでいるかどうか、またはその OBJECT に KEY エントリがあるかどうかをテストします。

```
pact> (contains 2 [1 2 3])
true

pact> (contains 'name { 'name: "Ted", 'age: 72 })
true

pact> (contains "foo" "foobar")
true
```

### drop<a id="sec-6-1-6"></a>

*count* `integer` *list* `<a[[<l>],string]>` *→* `<a[[<l>],string]>`

*keys* `[string]` *object* `object:<{o}>` *→* `object:<{o}>`

COUNT の数の値を LIST (または文字列) から除くか、KEYS にキーがあるエントリを OBJECT から除きます。COUNT が負の数の場合は、後ろから除きます。

```
pact> (drop 2 "vwxyz")
"xyz"

pact> (drop (- 2) [1 2 3 4 5])
[1 2 3]

pact> (drop ['name] { 'name: "Vlad", 'active: false})
{"active": false}
```

### enforce<a id="sec-6-1-7"></a>

*test* `bool` *msg* `string` *→* `bool`

TEST が false である場合、MSG を返してトランザクションが失敗するか、True を返します。

```
pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
<interactive>:0:0: Chaos reigns
```

### enforce-one<a id="sec-6-1-8"></a>

*msg* `string` *tests* `[bool]` *→* `bool`

TESTS を順番に実行します (副作用なしのコンテキストで、かつキーセットを施行)。すべてのテストが失敗した場合、トランザクションが失敗します。最初の成功時に短絡評価されます。

```
pact> (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
true
```

### enforce-pact-version<a id="sec-6-1-9"></a>

*min-version* `string` *→* `bool`

*min-version* `string` *max-version* `string` *→* `bool`

MIN-VERSION 以上、または MAX-VERSION 以下のランタイム pact バージョンを施行します。バージョンの値は、左側の数値が一致していれば一致しているものと判断されます。つまり、"2.2.3" が "2"、"2.2"、"2.2.3" のいずれかとして見なされます。

```
pact> (enforce-pact-version "2.3")
true
```

### filter<a id="sec-6-1-10"></a>

*app* `(x:<a> -> bool)` *list* `[<a>]` *→* `[<a>]`

すべての要素に APP を適用して LIST をフィルタリングします。APPの結果が True の場合に元の値が残されます。

```
pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```

### fold<a id="sec-6-1-11"></a>

*app* `(x:<a> y:<b> -> <a>)` *init* `<a>` *list* `[<b>]` *→* `<a>`

INIT から始め、 要素に APP を適用して LIST を絞り込んでいきます。

```
pact> (fold (+) 0 [100 10 5])
115
```

### format<a id="sec-6-1-12"></a>

*template* `string` *vars* `list` *→* `string`

`{}` を使用して、VARS を TEMPLATE に挿入します。

```
pact> (format "My {} has {}" ["dog" "fleas"])
"My dog has fleas"
```

### hash<a id="sec-6-1-13"></a>

*value* `<a>` *→* `string`

VALUE の BLAKE 2b 512 ビット ハッシュを計算します。文字列は直接変換され、その他の値は JSON 表現を使用して変換されます。

```
pact> (hash "hello")
"e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"

pact> (hash { 'foo: 1 })
"61d3c8775e151b4582ca7f9a885a9b2195d5aa6acc58ddca61a504e9986bb8c06eeb37af722ad848f9009053b6379677bf111e25a680ab41a209c4d56ff1e183"
```

### identity<a id="sec-6-1-14"></a>

*value* `<a>` *→* `<a>`

指定された値をそのまま返します。

```
pact> (map (identity) [1 2 3])
[1 2 3]
```

### if<a id="sec-6-1-15"></a>

*cond* `bool` *then* `<a>` *else* `<a>` *→* `<a>`

COND をテストし、True の場合は THEN を、それ以外の場合は ELSE を評価します。

```
pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
"Sanity prevails"
```

### length<a id="sec-6-1-16"></a>

*x* `<a[[<l>],string,object:<{o}>]>` *→* `integer`

X の長さを計算します。X にはリスト、文字列、オブジェクトのいずれかを指定できます。

```
pact> (length [1 2 3])
3

pact> (length "abcdefgh")
8

pact> (length { "a": 1, "b": 2 })
2
```

### list<a id="sec-6-1-17"></a>

*elems* `*` *→* `list`

ELEMS からリストを作成します。Pact 2.1.1 以降では非推奨です。代わりにリテラル リストがサポートされています。

```
pact> (list 1 2 3)
[1 2 3]
```

### list-modules<a id="sec-6-1-18"></a>

*→* `[string]`

ロードできるモジュールを示します。

### make-list<a id="sec-6-1-19"></a>

*length* `integer` *value* `<a>` *→* `[<a>]`

VALUE を LENGTH 回繰り返してリストを作成します。

```
pact> (make-list 5 true)
[true true true true true]
```

### map<a id="sec-6-1-20"></a>

*app* `(x:<b> -> <a>)` *list* `[<b>]` *→* `[<a>]`

LIST の要素に APP を適用し、新しいリストを返します。

```
pact> (map (+ 1) [1 2 3])
[2 3 4]
```

### pact-id<a id="sec-6-1-21"></a>

*→* `integer`

現在の pact の実行中に呼び出された場合は ID を返し、そうでない場合は失敗します。

### pact-version<a id="sec-6-1-22"></a>

*→* `string`

現在の pact の バージョンを取得します。

```
pact> (pact-version)
"2.5.0"
```

### read-decimal<a id="sec-6-1-23"></a>

*key* `string` *→* `decimal`

モジュールの引数のあるキーを少数としてパースします。

```lisp
(defun exec ()
  (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

### read-integer<a id="sec-6-1-24"></a>

*key* `string` *→* `integer`

モジュールの引数のあるキーを整数としてパースします。

```lisp
(read-integer "age")
```

### read-msg<a id="sec-6-1-25"></a>

*→* `<a>`

*key* `string` *→* `<a>`

メッセージ データからの KEY、または KEY が指定されていない場合はデータ本体自体を読み取ります。値は、String -> string、Number -> integer、Boolean -> bool、List -> list、Object -> object の規則に従って pact 型に強制します。

```lisp
(defun exec ()
  (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

### remove<a id="sec-6-1-26"></a>

*key* `string` *object* `object:<{o}>` *→* `object:<{o}>`

KEY のエントリを OBJECT から削除します。

```
pact> (remove "bar" { "foo": 1, "bar": 2 })
{"foo": 1}
```

### resume<a id="sec-6-1-27"></a>

*binding* `binding:<{y}>` *body* `*` *→* `<a>`

pact の前のステップ実行でイールドされたオブジェクト値にバインドする特殊形式です。

### reverse<a id="sec-6-1-28"></a>

*list* `[<a>]` *→* `[<a>]`

リストを逆転させます。

```
pact> (reverse [1 2 3])
[3 2 1]
```

### sort<a id="sec-6-1-29"></a>

*values* `[<a>]` *→* `[<a>]`

*fields* `[string]` *values* `[object:<{o}>]` *→* `[object:<{o}>]`

プリミティブ VALUES の単一型リストを並べ替えます。オブジェクトの場合 FIELDS リストを使用して並べ替えます。

```
pact> (sort [3 1 2])
[1 2 3]

pact> (sort ['age] [{'name: "Lin",'age: 30} {'name: "Val",'age: 25}])
[{"name": "Val", "age": 25} {"name": "Lin", "age": 30}]
```

### take<a id="sec-6-1-30"></a>

*count* `integer` *list* `<a[[<l>],string]>` *→* `<a[[<l>],string]>`

*keys* `[string]` *object* `object:<{o}>` *→* `object:<{o}>`

COUNT の数の値を LIST (または文字列) から取得するか、KEYS にキーがあるエントリを OBJECT から取得します。COUNT が負の数の場合は、最後から取得します。

```
pact> (take 2 "abcd")
"ab"

pact> (take (- 3) [1 2 3 4 5])
[3 4 5]

pact> (take ['name] { 'name: "Vlad", 'active: false})
{"name": "Vlad"}
```

### tx-hash<a id="sec-6-1-31"></a>

*→* `string`

現在のトランザクションのハッシュを文字列として取得します。

```
pact> (tx-hash)
"786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
```

### typeof<a id="sec-6-1-32"></a>

*x* `<a>` *→* `string`

X の型を文字列として返します。

```
pact> (typeof "hello")
"string"
```

### where<a id="sec-6-1-33"></a>

*field* `string` *app* `(x:<a> -> bool)` *value* `object:<{row}>` *→* `bool`

"filter" と "select" で使用して、APP を VALUE 内の FIELD に適用するユーティリティです。

```
pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
[{"name": "Juan", "age": 15}]
```

### yield<a id="sec-6-1-34"></a>

*OBJECT* `object:<{y}>` *→* `object:<{y}>`

OBJECT をイールドします。イールドされたオブジェクトは、次の pact ステップで "resume" を使って使用できます。オブジェクトはデータベースの行オブジェクトと同様、最上位レベルのみが "resume" でバインドでき、ネストされたオブジェクトは通常の JSON 値に変換されます。

```lisp
(yield { "amount": 100.0 })
```

## データベース<a id="sec-6-2"></a>

### create-table<a id="sec-6-2-1"></a>

*table* `table:<{row}>` *→* `string`

テーブル TABLE を作成します。

```lisp
(create-table accounts)
```

### describe-keyset<a id="sec-6-2-2"></a>

*keyset* `string` *→* `value`

KEYSET のメタデータを取得します。

### describe-module<a id="sec-6-2-3"></a>

*module* `string` *→* `value`

MODULE のメタデータを取得します。"name"、"hash"、"blessed"、"code"、"keyset" のフィールドを持つオブジェクトを返します。

```lisp
(describe-module 'my-module)
```

### describe-table<a id="sec-6-2-4"></a>

*table* `table:<{row}>` *→* `value`

TABLE のメタデータを取得します。"name"、"hash"、"blessed"、"code"、"keyset" のフィールドを持つオブジェクトを返します。

```lisp
(describe-table accounts)
```

### insert<a id="sec-6-2-5"></a>

*table* `table:<{row}>` *key* `string` *object* `object:<{row}>` *→* `string`

TABLE の KEY に OBJECT 列データのエントリを書き込みます。KEY に既にデータが存在する場合は失敗します。

```lisp
(insert 'accounts { "balance": 0.0, "note": "Created account."})
```

### keylog<a id="sec-6-2-6"></a>

*table* `table:<{row}>` *key* `string` *txid* `integer` *→* `[object]`

TXID 以降のトランザクションで TABLE の KEY に対して行われた更新を返します。結果はオブジェクトのリストで、それぞれのキーが各トランザクションの ID です。

```lisp
(keylog 'accounts "Alice" 123485945)
```

### keys<a id="sec-6-2-7"></a>

*table* `table:<{row}>` *→* `[string]`

TABLE のすべてのキーを返します。

```lisp
(keys 'accounts)
```

### read<a id="sec-6-2-8"></a>

*table* `table:<{row}>` *key* `string` *→* `object:<{row}>`

*table* `table:<{row}>` *key* `string` *columns* `[string]` *→* `object:<{row}>`

TABLE から の KEY に一致する行を読み取り、オブジェクトを返すか、単純に COLUMNS (指定されている場合) を返します。

```lisp
(read 'accounts id ['balance 'ccy])
```

### select<a id="sec-6-2-9"></a>

*table* `table:<{row}>` *where* `(row:object:<{row}> -> bool)` *→* `[object:<{row}>]`

*table* `table:<{row}>` *columns* `[string]` *where* `(row:object:<{row}> -> bool)` *→* `[object:<{row}>]`

各行に WHERE 関数 を適用してテーブルから行全体または COLUMNS を返します。

```lisp
(select people ['firstName, 'lastName] (where 'name (= "Fatima")))

(select people (where 'age (> 30)))
```

### txids<a id="sec-6-2-10"></a>

*table* `table:<{row}>` *txid* `integer` *→* `[integer]`

TABLE の TXID 以上のすべての txid 値を返します。

```lisp
(txids accounts 123849535)
```

### txlog<a id="sec-6-2-11"></a>

*table* `table:<{row}>` *txid* `integer` *→* `[value]`

トランザクション TXID で実行された TABLE へのすべての更新を返します。

```lisp
(txlog 'accounts 123485945)
```

### update<a id="sec-6-2-12"></a>

*table* `table:<{row}>` *key* `string` *object* `object:<{row}>` *→* `string`

TABLE の KEY に OBJECT 列データを書き込みます。 KEY にデータが存在しない場合は失敗します。

```lisp
(update 'accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })
```

### with-default-read<a id="sec-6-2-13"></a>

*table* `table:<{row}>` *key* `string` *defaults* `object:<{row}>` *bindings* `binding:<{row}>` *→* `<a>`

TABLE から KEY の行を読み取り、列を BINDING によって後続の本体ステートメントにバインドする特殊形式です。行が見つからない場合、DEFAULTS (一致するキー名を持つオブジェクト) から列を読み取ります。

```lisp
(with-default-read 'accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```

### with-read<a id="sec-6-2-14"></a>

*table* `table:<{row}>` *key* `string` *bindings* `binding:<{row}>` *→* `<a>`

TABLE から KEY の行を読み取り、列を BINDING によって後続の本体ステートメントにバインドする特殊形式です。

```lisp
(with-read 'accounts id { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```

### write<a id="sec-6-2-15"></a>

*table* `table:<{row}>` *key* `string` *object* `object:<{row}>` *→* `string`

TABLE の KEY に OBJECT 列データを書き込みます。

```lisp
(write 'accounts { "balance": 100.0 })
```

## 時刻<a id="sec-6-3"></a>

### add-time<a id="sec-6-3-1"></a>

*time* `time` *seconds* `decimal` *→* `time`

*time* `time` *seconds* `integer` *→* `time`

SECONDS を TIME に追加します。SECONDS は整数または小数です。

```
pact> (add-time (time "2016-07-22T12:00:00Z") 15)
"2016-07-22T12:00:15Z"
```

### days<a id="sec-6-3-2"></a>

*n* `decimal` *→* `decimal`

*n* `integer` *→* `decimal`

"add-time" で使用する日数 (N 日) です。

```
pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
"2016-07-23T12:00:00Z"
```

### diff-time<a id="sec-6-3-3"></a>

*time1* `time` *time2* `time` *→* `decimal`

TIME1 と TIME2 の差を秒数で計算します。

```
pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
23400
```

### format-time<a id="sec-6-3-4"></a>

*format* `string` *time* `time` *→* `string`

FORMAT を使用して TIME の形式を設定します。[サポートされている形式については、「時間形式」の文書を](https://pact-language.readthedocs.io/en/latest/pact-functions.html#time-formats)参照してください。

```
pact> (format-time "%F" (time "2016-07-22T12:00:00Z"))
"2016-07-22"
```

### hours<a id="sec-6-3-5"></a>

*n* `decimal` *→* `decimal`

*n* `integer` *→* `decimal`

"add-time" で使用する時間数 (N 時間) です。

```
pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
"2016-07-22T13:00:00Z"
```

### minutes<a id="sec-6-3-6"></a>

*n* `decimal` *→* `decimal`

*n* `integer` *→* `decimal`

"add-time" で使用する分数 (N 分) です。

```
pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
"2016-07-22T12:01:00Z"
```

### parse-time<a id="sec-6-3-7"></a>

*format* `string` *utcval* `string` *→* `time`

FORMAT を使用して UTCVAL から時刻を作成します。[サポートされている形式については、「時間形式」の文書を](https://pact-language.readthedocs.io/en/latest/pact-functions.html#time-formats)参照してください。

```
pact> (parse-time "%F" "2016-09-12")
"2016-09-12T00:00:00Z"
```

### time<a id="sec-6-3-8"></a>

*utcval* `string` *→* `time`

ISO8601 形式 (%Y-%m-%dT%H:%M:%SZ) を使用して UTCVAL から時刻を作成します。

```
pact> (time "2016-07-22T11:26:35Z")
"2016-07-22T11:26:35Z"
```

## 演算子<a id="sec-6-4"></a>

### !=<a id="sec-6-4-1"></a>

*x* `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *y* `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *→* `bool`

X が Y と等しくない場合、True です。

```
pact> (!= "hello" "goodbye")
true
```

### \*<a id="sec-6-4-2"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

X に Y を乗じます。

```
pact> (* 0.5 10.0)
5

pact> (* 3 5)
15
```

### +<a id="sec-6-4-3"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

*x* `<a[string,[<l>],object:<{o}>]>` *y* `<a[string,[<l>],object:<{o}>]>` *→* `<a[string,[<l>],object:<{o}>]>`

数値の加算、文字列/リストの連結、またはオブジェクトのマージを行います。

```
pact> (+ 1 2)
3

pact> (+ 5.0 0.5)
5.5

pact> (+ "every" "body")
"everybody"

pact> (+ [1 2] [3 4])
[1 2 3 4]

pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
{"bar": 2, "foo": 100}
```

### -<a id="sec-6-4-4"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

*x* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

X の符号を反転するか、X から Y を引きます。

```
pact> (- 1.0)
-1.0

pact> (- 3 2)
1
```

### /<a id="sec-6-4-5"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

X を Y で割ります。

```
pact> (/ 10.0 2.0)
5

pact> (/ 8 3)
2
```

### <<a id="sec-6-4-6"></a>

*x* `<a[integer,decimal,string,time]>` *y* `<a[integer,decimal,string,time]>` *→* `bool`

X < Y の場合、True です。

```
pact> (< 1 3)
true

pact> (< 5.24 2.52)
false

pact> (< "abc" "def")
true
```

### <=<a id="sec-6-4-7"></a>

*x* `<a[integer,decimal,string,time]>` *y* `<a[integer,decimal,string,time]>` *→* `bool`

X <= Y の場合、True です。

```
pact> (<= 1 3)
true

pact> (<= 5.24 2.52)
false

pact> (<= "abc" "def")
true
```

### =<a id="sec-6-4-8"></a>

*x* `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *y* `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` *→* `bool`

X が Y に等しい場合、True です。

```
pact> (= [1 2 3] [1 2 3])
true

pact> (= 'foo "foo")
true

pact> (= { 1: 2 } { 1: 2})
true
```

### ><a id="sec-6-4-9"></a>

*x* `<a[integer,decimal,string,time]>` *y* `<a[integer,decimal,string,time]>` *→* `bool`

X > Y の場合、True です。

```
pact> (> 1 3)
false

pact> (> 5.24 2.52)
true

pact> (> "abc" "def")
false
```

### >=<a id="sec-6-4-10"></a>

*x* `<a[integer,decimal,string,time]>` *y* `<a[integer,decimal,string,time]>` *→* `bool`

X >= Y の場合、True です。

```
pact> (>= 1 3)
false

pact> (>= 5.24 2.52)
true

pact> (>= "abc" "def")
false
```

### ^<a id="sec-6-4-11"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

X の Y 乗です。

```
pact> (\^ 2 3)
8
```

### abs<a id="sec-6-4-12"></a>

*x* `decimal` *→* `decimal`

*x* `integer` *→* `integer`

X の絶対値です。

```
pact> (abs (- 10 23))
13
```

### and<a id="sec-6-4-13"></a>

*x* `bool` *y* `bool` *→* `bool`

短絡評価のある、論理演算の「論理積」です。

```
pact> (and true false)
false
```

### and?<a id="sec-6-4-14"></a>

*a* `(x:<r> -> bool)` *b* `(x:<r> -> bool)` *value* `<r>` *→* `bool`

VALUE を A と B に適用した結果に論理積を行います。

```
pact> (and? (> 20) (> 10) 15)
false
```

### ceiling<a id="sec-6-4-15"></a>

*x* `decimal` *prec* `integer` *→* `decimal`

*x* `decimal` *→* `integer`

切り上げによって、小数 X を整数で表すか、精度 PREC の小数で表します。

```
pact> (ceiling 3.5)
4

pact> (ceiling 100.15234 2)
100.16
```

### exp<a id="sec-6-4-16"></a>

*x* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

X の指数関数です。

```
pact> (round (exp 3) 6)
20.085537
```

### floor<a id="sec-6-4-17"></a>

*x* `decimal` *prec* `integer` *→* `decimal`

*x* `decimal` *→* `integer`

切り下げによって、小数 X を整数または精度 PREC の小数で表します。

```
pact> (floor 3.5)
3

pact> (floor 100.15234 2)
100.15
```

### ln<a id="sec-6-4-18"></a>

*x* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

X の自然対数です。

```
pact> (round (ln 60) 6)
4.094345
```

### log<a id="sec-6-4-19"></a>

*x* `<a[integer,decimal]>` *y* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

*x* `<a[integer,decimal]>` *y* `<b[integer,decimal]>` *→* `decimal`

Y を底とした X の対数です。

```
pact> (log 2 256)
8
```

### mod<a id="sec-6-4-20"></a>

*x* `integer` *y* `integer` *→* `integer`

X モジュロ Y です。

```
pact> (mod 13 8)
5
```

### not<a id="sec-6-4-21"></a>

*x* `bool` *→* `bool`

論理演算の「否定」です。

```
pact> (not (> 1 2))
true
```

### not?<a id="sec-6-4-22"></a>

*app* `(x:<r> -> bool)` *value* `<r>` *→* `bool`

VALUE に対し APP関数を適用した結果を論理否定します。

```
pact> (not? (> 20) 15)
false
```

### or<a id="sec-6-4-23"></a>

*x* `bool` *y* `bool` *→* `bool`

短絡評価のある、論理演算の「論理和」です。

```
pact> (or true false)
true
```

### or?<a id="sec-6-4-24"></a>

*a* `(x:<r> -> bool)` *b* `(x:<r> -> bool)` *value* `<r>` *→* `bool`

VALUE を A と B に適用した結果に論理和を行います。

```
pact> (or? (> 20) (> 10) 15)
true
```

### round<a id="sec-6-4-25"></a>

*x* `decimal` *prec* `integer` *→* `decimal`

*x* `decimal` *→* `integer`

銀行丸めによって、小数 X を整数または精度 PREC の小数で表します。

```
pact> (round 3.5)
4

pact> (round 100.15234 2)
100.15
```

### sqrt<a id="sec-6-4-26"></a>

*x* `<a[integer,decimal]>` *→* `<a[integer,decimal]>`

X の平方根。

```
pact> (sqrt 25)
5
```

## キーセット<a id="sec-6-5"></a>

### define-keyset<a id="sec-6-5-1"></a>

*name* `string` *keyset* `string` *→* `string`

KEYSET を使用して NAME というキーセットを定義します。キーセット NAME が既に存在する場合、新しい値に更新する前にキーセットが施行されます。

```lisp
(define-keyset 'admin-keyset (read-keyset "keyset"))
```

### enforce-keyset<a id="sec-6-5-2"></a>

*keyset-or-name* `<k[string,keyset]>` *→* `bool`

BODY の実行前に、メッセージ キーに対して KEYSET-OR-NAME を施行する特殊形式です。KEYSET-OR-NAME はキーセット名のシンボル、またはキーセット オブジェクトのいずれかです。

```lisp
(with-keyset 'admin-keyset ...)

(with-keyset (read-keyset "keyset") ...)
```

### keys-2<a id="sec-6-5-3"></a>

*count* `integer` *matched* `integer` *→* `bool`

キーセット内で 2 つ以上のキーが一致しているかどうかを評価するキーセット述語関数。

```
pact> (keys-2 3 1)
false
```

### keys-all<a id="sec-6-5-4"></a>

*count* `integer` *matched* `integer` *→* `bool`

キーセット内ですべてのキーが一致しているかどうかを評価するキーセット述語関数。

```
pact> (keys-all 3 3)
true
```

### keys-any<a id="sec-6-5-5"></a>

*count* `integer` *matched* `integer` *→* `bool`

キーセット内に一致するキーが (1 つ以上) あるかどうかを評価するキーセット述語関数。

```
pact> (keys-any 10 1)
true
```

### read-keyset<a id="sec-6-5-6"></a>

*key* `string` *→* keyset

メッセージ データから KEY をキーセットとして読み取ります `({ “keys”: KEYLIST, “pred”: PREDFUN })` 。 PREDFUN は、キー述語でなければなりません。

```lisp
(read-keyset "admin-keyset")
```

## REPL 専用の関数<a id="sec-6-6"></a>

以下の関数は、インタラクティブな REPL、または .repl 拡張子を持つスクリプト ファイルに自動的にロードされます。これらは、ブロックチェーンベースの実行では利用できません。

### begin-tx<a id="sec-6-6-1"></a>

*→* `string`

*name* `string` *→* `string`

トランザクションを開始します。オプションで NAME を指定できます。

```lisp
(begin-tx "load module")
```

### bench<a id="sec-6-6-2"></a>

*exprs* `*` *→* `string`

EXPRS のベンチマークを実行します。

```lisp
(bench (+ 1 2))
```

### commit-tx<a id="sec-6-6-3"></a>

*→* `string`

トランザクションをコミットします。

```lisp
(commit-tx)
```

### env-data<a id="sec-6-6-4"></a>

*json* `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,value]>` *→* `string`

文字列としてエンコードするか、JSON に pact 型を変換して、トランザクション JSON データを設定します。

```
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```

### env-entity<a id="sec-6-6-5"></a>

*→* `string`

*entity* `string` *→* `string`

REPL 内で機密 ENTITY ID を設定するか、引数を指定せずに設定を解除します。以前の pact 実行状態はすべて排除されます。

```lisp
(env-entity "my-org")

(env-entity)
```

### env-gas<a id="sec-6-6-6"></a>

*→* `integer`

*gas* `integer` *→* `string`

ガスの状態を照会するか、GAS に設定します。

### env-gaslimit<a id="sec-6-6-7"></a>

*limit* `integer` *→* `string`

環境ガスの制限を LIMIT に設定します。

### env-gasprice<a id="sec-6-6-8"></a>

*price* `decimal` *→* `string`

環境ガスの価格を PRICE に設定します。

### env-gasrate<a id="sec-6-6-9"></a>

*rate* `integer` *→* `string`

定額の RATE をチャージするように、ガス モデルを更新します。

### env-hash<a id="sec-6-6-10"></a>

*hash* `string` *→* `string`

現在のトランザクション ハッシュを設定します。HASH には、有効な BLAKE2b 512 ビット ハッシュを指定する必要があります。

```
pact> (env-hash (hash "hello"))
"Set tx hash to e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"
```

### env-keys<a id="sec-6-6-11"></a>

*keys* `[string]` *→* `string`

トランザクション署名 KEYS を設定します。

```
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
```

### env-step<a id="sec-6-6-12"></a>

*→* `string`

*step-idx* `integer` *→* `string`

*step-idx* `integer` *rollback* `bool` *→* `string`

*step-idx* `integer` *rollback* `bool` *resume* `object:<{y}>` *→* `string`

pact ステップ状態を設定します。引数を指定しない場合は、ステップの設定が解除されます。STEP-IDX には、実行するステップ インデックスを設定します。ROLLBACK では、ロールバック式が存在する場合に、それを実行するかどうかを指定します。RESUME では、"resume" によって読み取る値を設定します。以前の pact 実行状態はすべて排除されます。

```
(env-step 1)

(env-step 0 true)
```

### expect<a id="sec-6-6-13"></a>

*doc* `string` *expected* `<a>` *actual* `<a>` *→* `string`

ACTUAL を評価し、それが EXPECTED に等しいことを確認します。

```
pact> (expect "Sanity prevails."4 (+ 2 2))
"Expect: success: Sanity prevails."
```

### expect-failure<a id="sec-6-6-14"></a>

*doc* `string` *exp* `<a>` *→* `string`

EXP を評価し、エラーの時のみ成功します。

```
pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
"Expect failure: success: Enforce fails on false"
```

### json<a id="sec-6-6-15"></a>

*exp* `<a>` *→* `value`

pact 式 EXP を JSON 値としてエンコードします。Pact 値は、API 出力で自動的に JSON で表されるため、これはテストの目的でのみ必要です。

```
pact> (json [{ "name": "joe", "age": 10 } {"name": "mary", "age": 25 }])
[{"age":10,"name":"joe"},{"age":25,"name":"mary"}]
```

### load<a id="sec-6-6-16"></a>

*file* `string` *→* `string`

*file* `string` *reset* `bool` *→* `string`

FILE をロードして評価します。オプションの RESET が true の場合は、repl 状態があらかじめリセットされます。

```lisp
(load "accounts.repl")
```

### pact-state<a id="sec-6-6-17"></a>

*→* `object`

以前の pact 実行がもたらした状態を確認します。返されるオブジェクトには、"yield" (結果をイールドするか、結果が存在しない場合は "false")、"step" (実行されたステップ)、"executed" (エンティティが一致しないためにステップがスキップされたかどうか) の各フィールドが含まれます。

```lisp
(pact-state)
```

### print<a id="sec-6-6-18"></a>

*value* `<a>` *→* `string`

文字列を印刷します。

### rollback-tx<a id="sec-6-6-19"></a>

*→* `string`

トランザクションをロールバックします。

```lisp
(rollback-tx)
```

### sig-keyset<a id="sec-6-6-20"></a>

*→* `keyset`

"keys-all" を述語として使用して、メッセージ署名に存在するキーからキーセットを構築するための関数です。

### typecheck<a id="sec-6-6-21"></a>

*module* `string` *→* `string`

*module* `string` *debug* `bool` *→* `string`

MODULE の型チェックを行います。オプションで DEBUG 出力を有効にすることができます。

### verify<a id="sec-6-6-22"></a>

*module* `string` *→* `string`

MODULE の形式検証を行います。

# Pact のプロパティ チェック システム<a id="sec-7"></a>

## 概要<a id="sec-7-1"></a>

Pact には、スマート コントラクトの作成者が Pact プログラムのプロパティ (指定) を表現し、自動的にチェックできる機能が搭載されています。

この Pact プロパティ チェック システムは、現在のスマート コントラクトのプログラミングの世界に存在する混乱と不確実性に対する当社の対応策として装備されたものです。人間がスマート コントラクトを作成する以上、ミスはつきものです。またスマート コントラクトを侵害しようとするあらゆる攻撃の手口を想定することは困難です。そこで当社は、形式検証の履歴が全くなくとも、コードが攻撃に対する防御力を備えていることを証明できる機能を用意しました。

例えば、任意の複雑な Pact プログラムにおいて、コントラクトの “管理者” にのみデータベースの編集権限を付与し、他者にはデータベースの読み取りのみを許可する場合には、コードをブロックチェーンに展開する前に、コードのプロパティを "静的に" 証明できます。

従来の単体テストでは、プログラムの動作は具体的な入力についてしか検証されず、作成者はそのケースがあらゆる入力に一般化できることを祈るしかありません。しかし Pact プロパティ チェック システムでは、可能な全ての入力に対してコードが自動的にチェックされます。

Pact でこれを実現するための機能として、作成者はデータベース テーブルの列に関する "スキーマの不変条件" を指定でき、また関数の引数と戻り値、キーセットの施行、データベース アクセス、および `enforce` の使用について、関数に関するプロパティを宣言して証明できます。

Pact のプロパティは "契約" の概念に対応 (注: "スマート コントラクト" とは異なる概念です) します。Pact の不変条件は、形式検証の世界の詳細型（refinement types）への第一歩となります。

この初期リリースでは、Pact 言語がまだ 100% サポートされておらず、プロパティ チェッカーの実装自体がまだ形式検証されていません。しかしこれはまだ第一歩に過ぎません。当社は引き続きあらゆる Pact プログラムのサポートを可能な限り拡張し、最終的には、精度が証明されたプロパティ チェッカーを提供するとともに、今後、作成者がスマート コントラクトに関するさらに高度なプロパティを表現できるように取り組みを続けていきます。

## プロパティおよびスキーマの不変条件の記述<a id="sec-7-2"></a>

以下に、Pact の実際のプロパティの例を示します。プロパティと共に、それに対応する関数の説明を付記しました。 関数は、キーセット施行の実装を `enforce-admin` という別の関数に委託するため、ユーザーはその実装について考える必要がありません。 このプロパティでは、ブロックチェーンに送信されたトランザクションが正常に実行されたとしたら、 `admins` というキーセットに対応する正しい署名がトランザクションに正しく施されたことになります。

```lisp
(defun read-account (id)
  @doc "Read data for account ID"
  @model (properties [(authorized-by 'admins)])

  (enforce-admin)
  (read 'accounts id ['balance 'ccy 'amount]))
```

ここでプロパティが、角かっこで囲って指定されていますが、これは、Pact で複数のプロパティを同時に定義できるためです。

```lisp
(properties [p1 p2 p3 ...])
```

次に、スキーマの不変条件の例を見てみましょう。次のスキーマを持つテーブルでプロパティ チェッカーが成功した場合、どのコードが実行されても、トークン残高が 0 より大きいという不変条件が常に維持されることになります。

```lisp
(defschema tokens
  @doc "token schema"
  @model (invariants [(> balance 0)])

  username:string
  balance:integer)
```

## プロパティ チェッカーのしくみ<a id="sec-7-3"></a>

Pact のプロパティ チェッカーは、STM (Satisfiability Modulo Theories) ソルバーで、言語のセマンティクスを実現することによって機能します。 これには、プログラムの数式を作成し、その数式の妥当性をテストする必要があります。 SMT ソルバーは、Pact コードに関して提起された命題について、それを偽ることができる変数と値の組み合わせが存在しないことを証明できます。 Pact は現在、Microsoft の [Z3 theorem prover](https://github.com/Z3Prover/z3/wiki) をプロパティ チェック システムの基盤として使用しています。

このような数式は、Pact モジュールの関数、それらの関数について提供されているプロパティ、モジュールのスキーマで宣言されている不変条件を組み合わせて作成されます。

Pact モジュール内の関数定義については、別の関数への後続の呼び出しがインライン化されています。プロパティをテストするには、その前にこのインライン化されたコードが型チェックに合格する必要があります。

スキーマの不変条件について、プロパティ チェッカーは帰納的なアプローチを取っています。 つまり、スキーマの不変条件が、現在のデータベースのデータについて "有効" であると仮定し、 データベースが変更されても、モジュール内のすべての関数でそれらの不変条件が維持されるかどうかを確認します。

## プロパティ チェッカーの使用方法<a id="sec-7-4"></a>

モジュール内で目的の不変条件とプロパティのアノテーションを指定した後、 `verify` を呼び出してプロパティ チェックを実行します。

```lisp
(verify 'module-name)
```

これによりコードの型チェックが実行され、それが正常に完了すると、すべての不変条件とプロパティがチェックされます。

## プロパティの表現<a id="sec-7-5"></a>

### 引数、戻り値、標準の演算子、比較演算子<a id="sec-7-5-1"></a>

プロパティでは、関数の引数を名前によって直接参照できます。また戻り値は、 `result` という名前で参照できます。

```lisp
(defun negate:integer (x:integer)
  @doc "negate a number"
  @model (properties [(= result (* -1 x))])

  (* x -1))
```

ここではまた、標準の算術演算子が、整数と小数に対して通常の Pact コードと同様に機能していることがわかります。

プロパティは標準の比較演算子に関しても定義できます。

```lisp
(defun abs:integer (x:integer)
  @doc "absolute value"
  @model (properties [(>= result 0)])

  (if (< x 0)
      (negate x)
    x))
```

### ブール演算子<a id="sec-7-5-2"></a>

Pact のプロパティ チェック言語は、標準のブール演算子である `and` 、 `or` 、 `not` に加え、 `when` の形式での論理包含をサポートします。ここで、 `(when x y)` は、 `(or (not x) y)` と同値です。以下では、3 つのプロパティが同時に定義されています。

```lisp
(defun negate:integer (x:integer)
  @doc "negate a number"
  @model (properties
          [(when (< x 0) (> result 0))
           (when (> x 0) (< result 0))
           (and
             (when (< x 0) (> result 0))
             (when (> x 0) (< result 0)))])

  (* x -1))
```

### トランザクションの中止と成功<a id="sec-7-5-3"></a>

デフォルトで、すべてのプロパティはテスト対象の関数の呼び出しを含めたトランザクションの成功を前提としています。例えば、以下のプロパティを考えてみましょう。

```lisp
(defun ensured-positive (val:integer)
  @doc "halts when passed a non-positive number"
  @model (properties [(!= result 0)])

  (enforce (> val 0) "val is not positive")
  val)
```

このプロパティは、 `enforce` を使用することで成功します。

ブロックチェーンの実行時に、 `enforce` 呼び出しが失敗すると、それに含まれているトランザクションが中止します。 `properties` は、トランザクションが成功することのみを前提としているため、それぞれの `enforce` 呼び出しが正常終了するための必要条件が成立していると想定されます。

### プロパティ API の詳細<a id="sec-7-5-4"></a>

プロパティで利用可能なすべての機能の詳細については、「[Property and Invariant Functions](http://pact-language.readthedocs.io/en/latest/pact-properties-api.html)」で API に関するドキュメントを参照してください。

## スキーマの不変条件の表現<a id="sec-7-6"></a>

スキーマの不変条件は、プロパティ定義で利用できる機能のうち、より限定的なサブセットによって記述されます。これらは具体的には、認証、データベース アクセス、トランザクションの成功と失敗、関数の引数と戻り値に関係しない機能です。不変条件の定義で利用可能なすべての機能の詳細については、API ドキュメントの「[Property and Invariant Functions](http://pact-language.readthedocs.io/en/latest/pact-properties-api.html)」を参照してください。

### キーセットの認証<a id="sec-7-6-1"></a>

Pact では、キーは定義済みの名前 (`define-keyset` で定義) で参照するか、値として渡すことができます。プロパティ チェック システムは、両方の方法によるキーセットの使用をサポートしています。

名前付きキーセットでは、すべての可能なコード パスがそのキーセットを施行する場合のみ、プロパティ `authorized-by` が成立します。

```lisp
(defun admins-only (action:string)
  @doc "Only admins or super-admins can call this function successfully."
  @model (properties
          [(or (authorized-by 'admins) (authorized-by 'super-admins))
           (when (== "create" action) (authorized-by 'super-admins))])

  (if (== action "create")
      (create)
    (if (== action "update")
        (update)
      (incorrect-action action))))
```

行レベルのキーセット施行の一般的なパターンとして、テーブルの行ごとにユーザーが割り当てられ、 各ユーザーの行にキーセットが格納されて、行の変更時に認証されることがあります。 このようなパターンが正しく実装されているかどうかは、 `row-enforced` プロパティを使用して確認できます。

以下のプロパティが合格するには、コードが `accounts` テーブルの ks 列に格納されている、変数 `name` によってキーが設定された行のキーセットを抽出し、 `enforce-keyset` を使用して施行する必要があります。

```lisp
(row-enforced 'accounts 'ks name)
```

`row-enforced` の実際の使用例については、以下の「単純な残高の移動の例」および「全称限定子と存在限定子」についてのセクションを参照してください。

### データベース アクセス<a id="sec-7-6-2"></a>

プロパティ言語には、データベースのテーブル アクセスを記述するための次のプロパティが用意されています。

-   `(table-written 'accounts)` - テーブル `accounts` の任意のセルへの書き込み
-   `(table-read 'accounts)` - テーブル `accounts` の任意のセルの読み取り
-   `(row-written 'accounts k)` - 変数 `k` によってキーが設定されている行の書き込み
-   `(row-read 'accounts k)` - 変数 `k` によってキーが設定されている行の読み取り

詳細については、以下の「全称限定子と存在限定子」で示す例を参照してください。

### 質量保存の法則と列の差分<a id="sec-7-6-3"></a>

状況によっては、トランザクションの前後で、列の値の合計が同じであることが望ましい場合があります。 言い換えれば、トランザクションの最後に、列に対するすべての更新額を合計すると差し引きゼロになる状況です。 このパターンを把握するために、 `conserves-mass` プロパティが用意されています。 このプロパティでは、テーブル名と列名を指定できます。

```lisp
(conserves-mass 'accounts 'balance)
```

このプロパティの使用例については、以下の「単純な残高の移動の例」を参照してください。

`conserves-mass` は実際には、 `column-delta` という別のプロパティの平凡な適用方法です。このプロパティは、トランザクションで列に対して行われたすべての変更の合計値を返します。したがって `(conserves-mass 'accounts 'balance)` は実際には、以下と同じです。

```lisp
(= 0 (column-delta 'accounts 'balance))
```

`column-delta` はまた、列の値が一定の割合で増加を続けていることを確認するために使用することもできます。

```lisp
(>= 0 (column-delta 'accounts 'balance))
```

もしくはトランザクション時に、設定された量だけ増加していることも確認できます。

```lisp
(= 1 (column-delta 'accounts 'balance))
```

`column-delta` は、トランザクションの前後 (`after - before`) の列の増加についてのみ定義されています。変化の絶対量ではありません。したがって、ここで 1 という値は、列の合計値が 1 だけ増加したことを示します。

### 全称限定子と存在限定子<a id="sec-7-6-4"></a>

ここまでの説明では、上記の `(row-enforced 'accounts 'ks key)` や `(row-written 'accounts key)` などの例で、 `key` という変数によって関数の引数のみを参照してきました。 しかし書き込みが可能なすべての行を扱う場合や、関数が複数の行を更新する場合はどうすればよいでしょうか。

そのような場合、全称限定子を使用して、条件を満たす "すべての" 行を扱うことができます。以下に例を示します。

```lisp
(properties
 [(forall (key:string)
     (when (row-written 'accounts key)
       (row-enforced 'accounts 'ks key)))])
```

このプロパティは、この関数によって書き込み可能なすべての行に対して、列 ks にあるキーセットをそれらの行に施行することを示しています。

同様に、対象となる可能性のあるすべてのキーを限定するのではなく、単にトランザクション時に読み取られる行が存在することを表す場合は、以下のように存在限定子を使用します。

```lisp
(properties
 [(exists (key:string)
    (row-read 'accounts key))])
```

全称限定子と存在限定子は、いずれも型アノテーションが必要であることに注意してください。

### プロパティの定義と再使用<a id="sec-7-6-5"></a>

`defproperty` を使用すると、プロパティをモジュール内で定義できます。

```lisp
(defmodule accounts
 @model
   [(defproperty conserves-mass
      (= (column-delta 'accounts 'balance) 0.0))

    (defproperty auth-required
      (authorized-by 'accounts-admin-keyset))]

 ; ...

 )
```

次にプロパティ名で参照して、この定義を関数レベルで使用します。

```lisp
(defun read-account (id)
 @model (property auth-required)

 ; ...

 )
```

## 単純な残高の移動の例<a id="sec-7-7"></a>

ここでは、特定のテーブルにある 2 つの口座間で残高を移動する関数の例について説明します。

```lisp
(defschema account
  @doc "user accounts with balances"

  balance:integer
  ks:keyset)

(deftable accounts:{account})
```

以下は、2 つの口座間で残高を移動するコードで、一見正しいようですがいくつかバグがあります。 別のプロパティを使用し、テーブルに不変条件に追加してこれらのバグを解消していきます。

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc "Transfer money between accounts"
  @model (properties [(row-enforced 'accounts 'ks from)])

  (with-read accounts *from* { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (update accounts *from* { "balance": (- from-bal amount) })
      (update accounts to { "balance": (+ to-bal amount) }))))
```

まず残高が 0 未満にならないという不変条件を追加します。

```lisp
(defschema account
  @doc "user accounts with balances"
  @model (invariants [(>= balance 0)])

  balance:integer
  ks:keyset)
```

次に、 `verify` を使用してこのモジュールのすべてのプロパティをチェックすると、 `amount` で -1 を渡すことにより、(残高が 0 の場合に) 正の残高という不変条件に違反できることがプロパティ チェッカーによって指摘されます。つまり、このコードでは、"送信者" が負の金額を移転すれば、他のユーザーの金銭を盗むことができます。そこで `(> amount 0)` を施行して、もう一度チェックしてみましょう。

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc "Transfer money between accounts"
  @model (properties [(row-enforced 'accounts 'ks from)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (enforce (> amount 0) "Non-positive amount")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to { "balance": (+ to-bal amount) }))))
```

この時点でプロパティ チェッカーによって、このコードの妥当性が検証されます。ここで別のプロパティ `(conserves-mass 'accounts 'balance)` を使用して、この関数が金銭を生み出したり、消してしまったりできないことを確認しましょう。

```lisp
(defun transfer (from:string to:string amount:integer)
  @doc "Transfer money between accounts"
  @model (properties
          [(row-enforced 'accounts 'ks from)
           (conserves-mass 'accounts 'balance)])

  (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
    (with-read accounts to { 'balance := to-bal }
      (enforce-keyset from-ks)
      (enforce (>= from-bal amount) "Insufficient Funds")
      (enforce (> amount 0) "Non-positive amount")
      (update accounts from { "balance": (- from-bal amount) })
      (update accounts to { "balance": (+ to-bal amount) }))))
```

ここで `verify` を実行すると、プロパティ チェッカーが再度バグを発見し、 `from` と `to` を同じ口座に設定すると、プロパティの偽装が可能であることがわかります。 これが本当であれば、このコードは何もないところから金銭を作り出すことになります。

この仕組みを解明するため、次の 2 つの `update` 呼び出しを検討してみましょう。これらの呼び出しでは、 `from` と `to` に同じ値が設定されるとともに、 `from-bal` と `to-bal` がいわゆる `previous-balance` に設定されています。

```lisp
(update accounts "alice" { "balance": (- previous-balance amount) })

(update accounts "alice" { "balance": (+ previous-balance amount) })
```

このシナリオでは、2 番目の `update` 呼び出しが `(+ previous-balance amount)` によって最初の呼び出しを完全に上書きすることがわかります。Alice はまさに `amount` の金額のトークンを無料で手に入れるのです。

そこで、もう 1 つ `enforce` を `((!= from to)` を指定して) 追加し、この予期しない動作を防止します。

```lisp
(defun transfer (from:string to:string amount:integer)
 @doc "Transfer money between accounts"
 @model (properties
         [(row-enforced 'accounts 'ks from)
          (conserves-mass 'accounts 'balance)])

 (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
   (with-read accounts to { 'balance := to-bal }
     (enforce-keyset from-ks)
     (enforce (>= from-bal amount) "Insufficient Funds")
     (enforce (> amount 0) "Non-positive amount")
     (enforce (!= from to) "Sender is the recipient")
     (update accounts from { "balance": (- from-bal amount) })
     (update accounts to { "balance": (+ to-bal amount) }))))
```

ここでプロパティ チェッカーを実行すると、以下のすべてが真実であることが検証されます。

-   送信側が金銭を移転するには、認証が必要である。
-   残高が 0 未満になることはない。
-   金銭を作り出したり、消したりすることはできない。

# プロパティと不変条件の関数<a id="sec-8"></a>

以下は、Pact の実行可能コードに限らず、プロパティや不変条件で使用できる関数です。 これらの関数がすべて、プロパティで使用できますが、不変条件で使用できるのは一部です。 通常、不変条件はデータの形状を記述することができ、プロパティはそれに加えて関数の入力と出力、 データベース操作を記述することができます。各関数について、それがプロパティ専用か、 プロパティと不変条件の両方で使用可能かを明記します。

## 数値演算子<a id="sec-8-1"></a>

### +<a id="sec-8-1-1"></a>

```lisp
(+ x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の加算。

不変条件またはプロパティでサポートされています。

### -<a id="sec-8-1-2"></a>

```lisp
(- x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の減算。

不変条件またはプロパティでサポートされています。

### \*<a id="sec-8-1-3"></a>

```lisp
(* x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の乗算。

不変条件またはプロパティでサポートされています。

### /<a id="sec-8-1-4"></a>

```lisp
(/ x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の除算。

不変条件またはプロパティでサポートされています。

### ^<a id="sec-8-1-5"></a>

```lisp
(^ x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の累乗。

不変条件またはプロパティでサポートされています。

### log<a id="sec-8-1-6"></a>

```lisp
(log b x)
```

-   入力: b、 *a* 型
-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

底を b とした x の対数。

不変条件またはプロパティでサポートされています。

### -<a id="sec-8-1-7"></a>

```lisp
(- x)
```

-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の符号の反転。

不変条件またはプロパティでサポートされています。

### sqrt<a id="sec-8-1-8"></a>

```
(sqrt x)
```

-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の平方根。

不変条件またはプロパティでサポートされています。

### ln<a id="sec-8-1-9"></a>

```lisp
(ln x)
```

-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

底を e とした整数または小数の対数。

不変条件またはプロパティでサポートされています。

### exp<a id="sec-8-1-10"></a>

```lisp
(exp x)
```

-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数および小数の指数関数。整数または小数 x の e 乗。

不変条件またはプロパティでサポートされています。

### abs<a id="sec-8-1-11"></a>

```lisp
(abs x)
```

-   入力: x、 *a* 型
-   戻り値: *a* 型
-   説明: *a* は integer 型または decimal 型

整数または小数の絶対値。

不変条件またはプロパティでサポートされています。

### round<a id="sec-8-1-12"></a>

```lisp
(round x)
```

-   入力: x、decimal 型
-   戻り値: integer 型

```lisp
(round x prec)
```

-   入力: x、decimal 型
-   入力: prec、integer 型
-   戻り値: integer 型

銀行丸めによって、小数 x を整数で表すか、精度 prec の小数で表します。

不変条件またはプロパティでサポートされています。

### ceiling<a id="sec-8-1-13"></a>

```lisp
(ceiling x)
```

-   入力: x、decimal 型
-   戻り値: integer 型

```lisp
(ceiling x prec)
```

-   入力: x、decimal 型
-   入力: prec、integer 型
-   戻り値: integer 型

切り上げによって小数 x を整数で表すか、精度 prec の小数で表します。

不変条件またはプロパティでサポートされています。

### floor<a id="sec-8-1-14"></a>

```lisp
(floor x)
```

-   入力: x、decimal 型
-   戻り値: integer 型

```lisp
(floor x prec)
```

-   入力: x、decimal 型
-   入力: prec、integer 型
-   戻り値: integer 型

切り下げによって小数 x を整数で表すか、精度 prec の小数で表します。

不変条件またはプロパティでサポートされています。

### mod<a id="sec-8-1-15"></a>

```lisp
(mod x y)
```

-   入力: x、integer 型
-   入力: y、integer 型
-   戻り値: integer 型

整数のモジュロ

不変条件またはプロパティでサポートされています。

## 論理演算子<a id="sec-8-2"></a>

### ><a id="sec-8-2-1"></a>

```lisp
(> x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型または decimal 型

x > y の場合、 True です

不変条件またはプロパティでサポートされています。

### <<a id="sec-8-2-2"></a>

```lisp
(< x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型または decimal 型

x < y の場合、True です

不変条件またはプロパティでサポートされています。

### >=<a id="sec-8-2-3"></a>

```lisp
(>= x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型または decimal 型

x >= y の場合、 True です

不変条件またはプロパティでサポートされています。

### <=<a id="sec-8-2-4"></a>

```lisp
(<= x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型または decimal 型

x <= y の場合、 True です

不変条件またはプロパティでサポートされています。

### =<a id="sec-8-2-5"></a>

```lisp
(= x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型、decimal 型、string 型、time 型、bool 型、object 型、keyset 型のいずれか

x = y の場合、 True です。

不変条件またはプロパティでサポートされています。

### !=<a id="sec-8-2-6"></a>

```lisp
(!= x y)
```

-   入力: x、 *a* 型
-   入力: y、 *a* 型
-   戻り値: bool 型
-   説明: *a* は integer 型、decimal 型、string 型、time 型、bool 型、object 型、keyset 型のいずれか

x != y の場合、True です

不変条件またはプロパティでサポートされています。

### and<a id="sec-8-2-7"></a>

```lisp
(and x y)
```

-   入力: x、bool 型
-   入力: y、bool 型
-   戻り値: bool 型

短絡論理積

不変条件またはプロパティでサポートされています。

### or<a id="sec-8-2-8"></a>

```lisp
(or x y)
```

-   入力: x、bool 型
-   入力: y、bool 型
-   戻り値: bool 型

短絡論理和

不変条件またはプロパティでサポートされています。

### not<a id="sec-8-2-9"></a>

```lisp
(not x)
```

-   入力: x、bool 型
-   戻り値: bool 型

論理否定

不変条件またはプロパティでサポートされています。

### when<a id="sec-8-2-10"></a>

```lisp
(when x y)
```

-   入力: x、bool 型
-   入力: y、bool 型
-   戻り値: bool 型

論理包含。(or (not x) y) と同値です。

不変条件またはプロパティでサポートされています。

## オブジェクト演算子<a id="sec-8-3"></a>

### at<a id="sec-8-3-1"></a>

```lisp
(at k o)
```

-   入力: k、string 型
-   入力: o、object 型
-   戻り値: bool 型

オブジェクトのプロジェクション

不変条件またはプロパティでサポートされています。

### +<a id="sec-8-3-2"></a>

```lisp
(+ x y)
```

-   入力: x、object 型
-   入力: y、object 型
-   戻り値: object 型

オブジェクトのマージ

不変条件またはプロパティでサポートされています。

## 文字列演算子<a id="sec-8-4"></a>

### length<a id="sec-8-4-1"></a>

```lisp
(length s)
```

-   入力: s、string 型
-   戻り値: integer 型

文字列長

不変条件またはプロパティでサポートされています。

### +<a id="sec-8-4-2"></a>

```lisp
(+ s t)
```

-   入力: s、string 型
-   入力: t、string 型
-   戻り値: string 型

文字列の連結

不変条件またはプロパティでサポートされています。

## 時間演算子<a id="sec-8-5"></a>

### add-time<a id="sec-8-5-1"></a>

```lisp
(add-time t s)
```

-   入力: t、time 型
-   入力: s、 *a* 型
-   戻り値: time 型
-   説明: *a* は integer 型または decimal 型

時間に秒数を追加します

不変条件またはプロパティでサポートされています。

## 定量演算子<a id="sec-8-6"></a>

### forall<a id="sec-8-6-1"></a>

```lisp
(forall (x:string) y)
```

-   バインド: x、 *a* 型
-   入力: y、 *r* 型
-   戻り値: *r* 型
-   説明: *a* は任意の型
-   説明: *r* は任意の型

全称限定された変数をバインドします

プロパティのみでサポートされています。

### exists<a id="sec-8-6-2"></a>

```lisp
(exists (x:string) y)
```

-   バインド: x、 *a* 型
-   入力: y、 *r* 型
-   戻り値: *r* 型
-   説明: *a* は任意の型
-   説明: *r* は任意の型

存在限定された変数をバインドします

プロパティのみでサポートされています。

## トランザクション演算子<a id="sec-8-7"></a>

### abort<a id="sec-8-7-1"></a>

```lisp
abort
```

-   bool 型

トランザクションが中止されるかどうかです。この関数は、トランザクションの成功を想定しない命題を表現する場合のみ有用です。 `property` によって定義される命題は、黙示的にトランザクションの成功を想定します。 当社では、今後この機能を使用する新しいモードを追加する予定です。この機能が必要な場合は当社までお知らせください。

プロパティのみでサポートされています。

### success<a id="sec-8-7-2"></a>

```lisp
success
```

-   bool 型

トランザクションが成功するかどうかです。この関数は、トランザクションの成功を想定しない命題を表現する場合のみ有用です。 `property` によって定義される命題は、黙示的にトランザクションの成功を想定します。 当社では、今後この機能を使用する新しいモードを追加する予定です。この機能が必要な場合は当社までお知らせください。

プロパティのみでサポートされています。

### result<a id="sec-8-7-3"></a>

```lisp
result
```

-   *r* 型
-   説明: *r* は任意の型

テスト対象の関数の戻り値です

プロパティのみでサポートされています。

## データベース演算子<a id="sec-8-8"></a>

### table-written<a id="sec-8-8-1"></a>

```lisp
(table-written t)
```

-   入力: t、 *a* 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型

分析対象の関数にテーブルが書き込まれるかどうか

プロパティのみでサポートされています。

### table-read<a id="sec-8-8-2"></a>

```lisp
(table-read t)
```

-   入力: t、 *a* 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型

分析対象の関数でテーブルが読み取られるかどうか

プロパティのみでサポートされています。

### cell-delta<a id="sec-8-8-3"></a>

```lisp
(cell-delta t c r)
```

-   入力: t、 *a* 型
-   入力: c、 *b* 型
-   入力: r、string 型
-   戻り値: *c* 型
-   説明: *a* は table 型または string 型
-   説明: *b* は column 型または string 型
-   説明: *c* は integer 型または decimal 型

トランザクションの前後におけるセルの値の差

プロパティのみでサポートされています。

### column-delta<a id="sec-8-8-4"></a>

```lisp
(column-delta t c)
```

-   入力: t、 *a* 型
-   入力: c、 *b* 型
-   戻り値: *c* 型
-   説明: *a* は table 型または string 型
-   説明: *b* は column 型または string 型
-   説明: *c* は integer 型または decimal 型

トランザクションの前後における列の合計値の差

プロパティのみでサポートされています。

### column-written<a id="sec-8-8-5"></a>

```lisp
(column-written t c)
```

-   入力: t、 *a* 型
-   入力: c、 *b* 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型
-   説明: *b* は column 型または string 型

分析対象の関数に列が書き込まれるかどうか

プロパティのみでサポートされています。

### column-read<a id="sec-8-8-6"></a>

```lisp
(column-read t c)
```

-   入力: t、 *a* 型
-   入力: c、 *b* 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型
-   説明: *b* は column 型または string 型

分析対象の関数に列が読み取られるかどうか

プロパティのみでサポートされています。

### row-read<a id="sec-8-8-7"></a>

```lisp
(row-read t r)
```

-   入力: t、 *a* 型
-   入力: r、string 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型

分析対象の関数で行が読み取られるかどうか

プロパティのみでサポートされています。

### row-written<a id="sec-8-8-8"></a>

```lisp
(row-written t r)
```

-   入力: t、 *a* 型
-   入力: r、string 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型

分析対象の関数に行が書き込まれるかどうか

プロパティのみでサポートされています。

### row-read-count<a id="sec-8-8-9"></a>

```lisp
(row-read-count t r)
```

-   入力: t、 *a* 型
-   入力: r、string 型
-   戻り値: integer 型
-   説明: *a* は table 型または string 型

トランザクションで行が読み取られる回数

プロパティのみでサポートされています。

### row-write-count<a id="sec-8-8-10"></a>

```lisp
(row-write-count t r)
```

-   入力: t、 *a* 型
-   入力: r、string 型
-   戻り値: integer 型
-   説明: *a* は table 型または string 型

トランザクションで行が書き込まれる回数

プロパティのみでサポートされています。

## 許可演算子<a id="sec-8-9"></a>

### authorized-by<a id="sec-8-9-1"></a>

```lisp
(authorized-by k)
```

-   入力: k、string 型
-   戻り値: bool 型

分析対象の関数によって名前付きキーセットが施行されるかどうか

プロパティのみでサポートされています。

### row-enforced<a id="sec-8-9-2"></a>

```lisp
(row-enforced t c r)
```

-   入力: t、 *a* 型
-   入力: c、 *b* 型
-   入力: r、string 型
-   戻り値: bool 型
-   説明: *a* は table 型または string 型
-   説明: *b* は column 型または string 型

分析対象の関数によって行内のキーセットが施行されるかどうか

プロパティのみでサポートされています。
