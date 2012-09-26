{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson (object)
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Web.Scotty
import           Network.Wai (Request(..))
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Rewrite
import           Network.Wai.Middleware.RequestLogger
import           Data.Monoid (mconcat, mappend)
import           Text.Markdown.Discount (markdown)
import           System.FilePath ((</>))
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.Environment (getArgs)
import           Text.Hastache
import qualified Data.Text as T
import           Data.Char (toLower, ord)
import           Data.Time.Format (formatTime)
import           Data.Time.Calendar (fromGregorian, Day)
import           System.Locale (defaultTimeLocale)
import           Data.Maybe (catMaybes)

markdownText a = TLE.decodeUtf8 $ LB.fromChunks [markdown a]

data TemplateSet = TemplateSet {
      postTemplate :: Template
    , homeTemplate :: Template
    }

type Template = (MuContext IO -> IO LB.ByteString)

data Config = Config {
      base :: String
    , port :: Int
    }

getConfig (base:port:[]) = Config base (read port)
getConfig _ = error "wrong number of arguments to hobo; use `hobo BASE_DIR PORT`"

templateDir = "templates"

readTemplatesOrError base = do
    post <- B.readFile (base </> templateDir </> "post.html")
    home <- B.readFile (base </> templateDir </> "home.html")
    return $ TemplateSet
        (hastacheStr ourConfig post)
        (hastacheStr ourConfig home)
  where
    ourConfig = defaultConfig {
                      muTemplateFileDir=Just includeDir
                    , muTemplateFileExt=Just ".html"
        }
    includeDir = base </> templateDir

data Post = Post (HM.HashMap B.ByteString B.ByteString) B.ByteString
parsePost :: B.ByteString -> Post
parsePost s =
    case Atto.parse parseHeaders s of
        Atto.Fail _ _ e -> error $ "parse error: " ++ e
        Atto.Partial _ -> error $ "parse error: only partial input while reading headers"
        Atto.Done left hs -> Post hs left

makeDate year month day = do
    fromGregorian iyear imonth iday
  where
    iyear = read year
    imonth = read month
    iday = read day

createDateDisplay day =
    formatTime defaultTimeLocale "%A, %B %e '%y" day

rewritePostStatic :: Config -> [T.Text] -> a -> IO [T.Text]
rewritePostStatic cfg parts@("static":x) _ = return parts
rewritePostStatic cfg parts@(y:m:d:dn:"":[]) _ = return parts
rewritePostStatic cfg parts@(y:m:d:dn:fn:[]) _ = do
    case yearLike y of
        True -> do
            let nm = T.concat [y, "-", m, "-", d, "-", dn]
            return $ ["posts", nm, fn]
        False -> return parts
  where
    yearLike p = (T.length p == 4) && (all (\c-> c `elem` ['0'..'9']) $ T.unpack p)
rewritePostStatic cfg p _ = return p

makePostPath cfg year month day name file =
    let dn = mconcat [year, "-", month, "-", day, "-", name] in
    (base cfg) </> "posts" </> dn </> file

postPage ts cfg = do
    url <- fmap rawPathInfo request
    when (not $ B.last url == '/') $
        redirect (TLE.decodeUtf8 $ LB.fromChunks [B.concat ["/", url, "/"]])
    year <- param "year"
    month <- param "month"
    day <- param "day"
    name <- param "name"
    let fullpath = makePostPath cfg year month day name "post.md"
    exists <- liftIO $ doesFileExist fullpath
    let dt = makeDate year month day
    case exists of
        True -> do
            c <- liftIO $ B.readFile fullpath
            let Post headers body = parsePost c
            let mod = markdownText body
            out <- liftIO $ (postTemplate ts) (sub headers mod dt)
            html $ TLE.decodeUtf8 out
        False -> next
  where
    sub _ _ d "date" = MuVariable (createDateDisplay d)
    sub _ b _ "body" = MuVariable b
    sub h _ _ k =
        case HM.lookup k h of
            Just v -> MuVariable v
            Nothing -> MuNothing

data PostFile = PostFile String Day String

parseParts f =
    case Atto.parse parseFilename f of
        Atto.Fail _ _ e -> Nothing
        Atto.Partial _ -> Nothing
        Atto.Done left p@(PostFile _ _ (n:_)) ->
            if (n == '_') then Nothing else Just p

parseFilename = do
    year <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    month <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    day <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    name <- fmap B.unpack $ Atto.takeWhile (not . Atto.isEndOfLine . fromIntegral . ord)
    let date = makeDate year month day
    let url = concat ["/", year, "/", month, "/", day, "/", name, "/"]
    return $ PostFile url date name

home ts cfg = do
    rpaths <- liftIO $ getDirectoryContents (base cfg </> "posts")
    let paths = (reverse . sort) rpaths
    let allPosts = catMaybes $ map (parseParts . B.pack . (flip (++) "\n")) paths
    out <- liftIO $ (homeTemplate ts) (sub allPosts)
    html $ TLE.decodeUtf8 out
  where
    sub posts "posts" = MuList $ map postCompleter posts
    sub posts o = MuNothing
    postCompleter (PostFile u d n) "url" = MuVariable u
    postCompleter (PostFile u d n) "date" = MuVariable (createDateDisplay d)
    postCompleter (PostFile u d n) "name" = MuVariable n

main = do
    args <- getArgs
    let cfg = getConfig args
    ts <- readTemplatesOrError (base cfg)
    scotty (port cfg) $ do
        middleware logStdoutDev
        middleware $ rewrite $ rewritePostStatic cfg
        middleware $ staticPolicy
            (noDots >-> (hasPrefix "static/" <|> (hasPrefix "posts/" >-> contains ".")) 
            >-> addBase (base cfg))
        get "/:year/:month/:day/:name" (postPage ts cfg)
        get "/" (home ts cfg)

parseHeaders :: Atto.Parser (HM.HashMap B.ByteString B.ByteString)
parseHeaders = do
    kvs <- Atto.many1 parseHeader
    Atto.skipSpace
    return $ HM.fromList kvs
  where
    parseHeader = do
        nm <- Atto.takeWhile (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
        _ <- Atto.char ':'
        Atto.skipSpace
        val <- Atto.takeWhile (\c -> not $ c `elem` "\r\n")
        Atto.takeWhile (\c -> c `elem` "\r\n")
        return (B.map toLower nm, val)
