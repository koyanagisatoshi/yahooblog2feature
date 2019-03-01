# Haskell
本プログラムは、Haskell-exeはTSV形式のYahoo!ブログから収集したブログ記事を、
素性ファイル (libsvm_formatter.prlで解釈できる形式) に変換します。
第一引数はinputのTSVファイルパス。
第二引数はoutputの素性ファイルパスです。
なお、mecab にパスが通ってないと失敗します。
コマンド例：./Haskell-exe blog.age.test.tsv blog.age.test.sosei

