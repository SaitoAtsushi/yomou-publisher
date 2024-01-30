# Yomou publisher

## 概要

小説投稿サイト「小説家になろう」から ePub 形式の電子書籍を作成するスクリプトです。

## 要求

* Gauche のバージョン 0.9.14
* [Gauche-epub](https://github.com/SaitoAtsushi/Gauche-epub)
* [Gauche-zip-archive](https://github.com/SaitoAtsushi/Gauche-zip-archive)

## コマンド

以下の例のような形式でコマンドに続けてオプションと小説IDを渡します。

```
ypub.scm -v n0126r
```

## オプション

### -v, --vertical

生成される ePub が縦書きモードになります。

### -n, --noimage

生成される ePub に画像を含めません。
