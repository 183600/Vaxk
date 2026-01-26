{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between left right str = do
    -- 1. breakOn 在第一个出现 left 的地方分割，返回 (before, after)
    let (before, rest) = T.breakOn left str
    
    -- 2. 如果 rest 就是 left (即没找到，或者 left 在末尾)，则返回 Nothing
    -- 注意：如果 left 是空字符串，T.breakOn 会返回原字符串，这里需要小心处理
    if T.null rest || rest == left 
        then Nothing
        else do
            -- 3. 去掉 left，剩下的部分
            let afterLeft = T.drop (T.length left) rest
            
            -- 4. 在剩下的部分里找 right
            let (middle, endPart) = T.breakOn right afterLeft
            
            -- 5. 检查是否找到了 right
            if T.null endPart 
                then Nothing 
                else Just middle

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
  , Import { name :: Maybe Text}
  , Func { name :: Maybe Text}
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

parseLine :: V.Vector Text Token -> [Text] -> [Text] -> Int -> [Token]
parseLine _ [] _ _= (Nothing,Text)
parseLine h fullList@("package":xs) a b = parseType Package fullList
parseLine h fullList@("import":xs) a b = parseType Import fullList
parseLine h fullList@("func":xs) a b = parseType Func fullList
parseLine h (x:xs) a b
  | a[b-1] == "package" = h V.// [(V.length , Package{name=a !! $ length a})]
  | a[b:1] == "import" = h V.// [(V.length , Import{name=parserGoString a ['"']})]
  | a[b-1] == "func" = T.break (=='(') x
  where
    parseType constructor fullList = parseLine (V.snoc h constructor{name=Nothing}) xs fullList a+1

parser' :: V.Vector Text Token -> [[Text]] -> [Maybe Token]
parser' _ [] = (Nothing,Text)
parser' h (x:xs) = parseLine V.fromList[] (x <> parser' xs) (x <> parser' xs) 0

parser :: Text -> ()
parser x = () 
  where
    parser' V.fromList[] x

parserGoString :: [Text] -> [Text] -> Maybe Text
parserGoString a [] = Nothing
parserGoString a (x:xs) = content
  where
    case between x x (a !! $ length a) of
        Just result -> let content = result -- 输出: Target Data
        Nothing     -> let content = parserGoString a xs

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

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
