module Main where

import Lib
import System.Environment (getArgs)
import System.IO
import Data.List.Split (splitOn)
import Data.List (nub)
import Text.Regex.Base
import Text.Regex.Posix
import Text.Regex
import System.Process

-- データ形式の変換用関数 TSV -> hTSV
type HTSV = [String]
tsv2htsv :: String -> [HTSV]
tsv2htsv s = [ splitOn "\t" s' | s' <- lines s]

-- データ形式の変換用関数 hTSV -> TSV
htsv2tsv :: [HTSV] -> String
htsv2tsv ss = unlines [ untab s | s <- ss ]
  where
    untab :: HTSV -> String
    untab [s1,s2,s3] = s1 ++ "\t" ++ s2 ++ "\t" ++ s3
    untab [s1,s2] = s1 ++ "\t" ++ s2
    untab [s1] = s1

-- mecab 出力形式を EOS 出力ごとに、リスト形式[で分割する。
splitOnEOS :: String -> [String]
splitOnEOS s =  splitOn "EOS\n" s

tab2Pair :: String -> (String, [String])
tab2Pair s = (x, (splitOn "," y))
  where x:y:[] = splitOn "\t" s


-- 正規表現で文字列を置換する関数
replace :: String -> String -> String -> String
replace r s t = subRegex (mkRegex r) s t

-- URL, 特殊文字を削除する関数
cleanse_URL_SpecialChar :: HTSV -> HTSV
cleanse_URL_SpecialChar [s1,s2,s3]= [s1,s2,(f s3)]
  where f s = (replace "(https?|ftp)(:\\/\\/[-_.!~*\\\'()a-zA-Z0-9;\\/?:\\@&=+\\$,%#]+)|&[#0-9a-zA-Z]+;" s "" )

-- ":" を "：" に置換する関数
cleanseColumn :: String -> String
cleanseColumn s = (replace ":" s "：")

type IDorAGEorKIHONKEIs =  (String, [String])
deleteKigou :: [IDorAGEorKIHONKEIs] -> [IDorAGEorKIHONKEIs]
deleteKigou xys = [ xy | xy <- xys, not (isKigou xy)]
  where
    isKigou (x,ss) | (ss !! 0) == "記号" = True
                   | otherwise          = False

type AGE_KIHONKEIs = (Int, [String])
deleteDuplication2Age_Kihonkeis :: [IDorAGEorKIHONKEIs] -> AGE_KIHONKEIs
deleteDuplication2Age_Kihonkeis ss
  | length ss == 0 = (0, ["この行はMain.hs::deleteDuplication2Age_Kihonkeisにより出力された不要なデータです"])
  | length ss == 1 = (0, ["この行はMain.hs::deleteDuplication2Age_Kihonkeisにより出力された不要なデータです"])
  | otherwise      = ((read age :: Int) , (unduplicate kihonkeis))
      where
      age = fst (ss!!1)
      kihonkeis = fst $ unzip (drop 2 ss) -- 最初の２要素（IDと年齢) をのぞくために drop 2 する。
      unduplicate ss = nub ss

getAgeKihonkeis :: [IDorAGEorKIHONKEIs] -> AGE_KIHONKEIs
getAgeKihonkeis []  = (0, ["この行はMain.hs::getAgeKihonkeisにより出力された不要なデータです。"])
getAgeKihonkeis xys = (age, kihonkeis)
  where
    xs = (fst (unzip xys))
    ys = (snd (unzip xys))
    age   = read (xs !! 1) :: Int
    kihonkeis = drop 2 [ if (ss !! 6) == "*" then x else (ss !! 6) | (x,ss) <- xys ]

showFeature :: AGE_KIHONKEIs -> String
showFeature (i,xs) = show i ++ "\t" ++ unwords xs

-- 環境変数
logdir = "log/"
before_mecab = logdir ++ "tmp_mecab_before"
after_mecab = logdir ++ "tmp_mecab_after"
haskell_exe_log = "Haskell-exe.log"

main :: IO ()
main = do

  system ("echo '============================================================'")
  system ("echo '本プログラムは、Haskell-exeはTSV形式のYahoo!ブログから収集したブログ記事を、'")
  system ("echo '素性ファイル (libsvm_formatter.prlで解釈できる形式) に変換します。'")
  system ("echo '第一引数はinputのTSVファイルパス。'")
  system ("echo '第二引数はoutputの素性ファイルパスです。'")
  system ("echo 'なお、mecab にパスが通ってないと失敗します。'")
  system ("echo 'コマンド例：./Haskell-exe blog.age.test.tsv blog.age.test.sosei'")
  system ("echo '============================================================'")
  system ("echo '処理開始：' > " ++ logdir ++ haskell_exe_log ++ ";" ++ "date >> " ++ logdir ++ haskell_exe_log)
  system ("echo 'プログラムを実行します。 (プログレスバーは出力されません。)'")
  system ("mkdir -p log")
  args <- getArgs -- 低: 引数チェック
  handle_in <- openFile (args !! 0) ReadMode
  handle_out <- openFile (args !! 1) WriteMode

  handle_before_mecab <- openFile before_mecab WriteMode
  tsv_string <- hGetContents handle_in
  hPutStr handle_before_mecab (htsv2tsv ([cleanse_URL_SpecialChar htsv | htsv <- tsv2htsv tsv_string ]))
  hClose  handle_before_mecab

  system ("mecab -b 81920 " ++ before_mecab ++ " > " ++ after_mecab) -- 低:system is obsolete -- mecabがインストールされてないと正常に終了しない。

  handle_after_mecab <- openFile after_mecab ReadMode
  s <- hGetContents handle_after_mecab
  hPutStr handle_out $ unlines [ showFeature (deleteDuplication2Age_Kihonkeis (deleteKigou xys)) |
                       xys  <- [[ tab2Pair s'' | s'' <- lines s' ] | s' <- splitOnEOS (cleanseColumn s)]]
  hClose handle_after_mecab

  system ("echo '処理終了：' >> " ++ logdir ++ haskell_exe_log ++ ";" ++ "date >> " ++ logdir ++ haskell_exe_log)
  system ("echo 'プログラムが最後まで実行されました。出力された素性ファイルを確認してください。。'")
  system ("echo 'なお、実行ログは " ++ logdir ++ " 配下の " ++ haskell_exe_log ++ " を確認してください。。'")
  hClose handle_in
  hClose handle_out
