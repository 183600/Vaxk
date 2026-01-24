module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- 1. 定义你的数据类型，表示CLI的参数结构
data Options = Options
  { optCommand :: Command
  } deriving (Show)

data Command
  = Run { path :: Text}
  | Build { path :: Text}
  deriving (Show)

data Token
  = Package { name :: Maybe Text}
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
    -- 3. 逐行处理 (例如打印行号和内容)
      -- 方案：遍历每一行，对每一行 words，然后 flatten（压平），最后处理
      let allWords = map T.words linelist
      -- allWords 的类型是 [Text]
      -- 使用 mapM_ 执行 IO
      mapM_ parser allWords

parseChar :: Char -> ()
parseChar ' '

parseLine :: V.Vector Text Token -> [Text] -> [Text] -> Int -> [Token]
parseLine _ [] _ _= (Nothing,Text)
parseLine h ("package":xs) a b = parseLine (V.snoc Package{name=Nothing} h) xs ("package"<>xs) a+1
parseLine h (x:xs) a b
  | a[b-1] == "package" =   

parser' :: V.Vector Text Token -> [[Text]] -> [Maybe Token]
parser' _ [] = (Nothing,Text)
parser' h (x:xs) = parseLine V.fromList[] (x <> parser' xs) (x <> parser' xs) 0

parser :: Text -> ()
parser x = () 
  where
    parser' V.fromList[] x

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
