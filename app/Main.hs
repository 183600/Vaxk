module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

-- 1. 定义你的数据类型，表示CLI的参数结构
data Options = Options
  { optCommand :: Command
  } deriving (Show)

data Command
  = Run { path :: Text}
  | Build { path :: Text}
  deriving (Show)

data Token
  = Package { name :: Text}
  deriving (Show)

-- 2. 实现具体的业务逻辑
runApp :: Options -> IO ()
runApp (Options cmd) = case cmd of
  Run x -> print $ x
  Build x -> print $ x
    where
      contents <- TIO.readFile "test.txt"
    -- 2. 将内容按行分割
      let lineList = T.lines contents
      let myMap = HashMap.fromList[] 
    -- 3. 逐行处理 (例如打印行号和内容)
      map parseLine $ T.words linelist-- (zip [1..] linelist)

parseChar :: Char -> ()
parseChar ' '

parseLinePart :: Text -> (Maybe Token,Text)
parseLinePart "package" = (Just Package{name=""},"package")
parseLinePart x = (Nothing,x)

parseLine' :: [(Int,Text)] -> [Maybe Token]
parseLine' [] = (Nothing,Text)
parseLine' (x:xs)
  | 

parseLine :: Text -> ()
parseLine x = () 
  where
    parseLine' $ T.words x

-- 解析子命令
runParser :: Parser Command
runParser = Run
  <$> argument auto (metavar "路径")
  -- <*> argument auto (metavar "NUM2")

-- 解析子命令
runParser :: Parser Command
runParser = Build
  <$> argument auto (metavar "路径")

-- 主 Parser
optionsParser :: Parser Options
optionsParser = Options
  <$> subparser
      (command "run"  (info runParser (progDesc "运行代码"))
      (command "build"  (info runParser (progDesc "编译代码"))
      )

-- 4. 程序入口
main :: IO ()
main = do
  -- execParser 会自动处理 --help 和 --version
  opts <- execParser (info (optionsParser <**> helper) fullDesc)
  runApp opts
