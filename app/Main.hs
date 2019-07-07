{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Monoid((<>))

import Network.Socket (withSocketsDo)
import Control.Exception (bracket, catches, Handler(..))

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Database.Tds.Message
import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Connection as MSSQL
import qualified Database.MSSQLServer.Query as MSSQL

import Data.Time (UTCTime(..))
import Data.UUID.Types (UUID(..))
import Data.Word (Word16)

import Control.Monad (forM_)


main :: IO ()
main = do
  let info = defaultConnectInfo { connectHost = "192.168.0.1"
                                , connectPort = "1433"
                                , connectDatabase = "some_database"
                                , connectUser = "some_user"
                                , connectPassword = "some_password"
--                                , connectEncryption = 0x02 -- [MEMO] 0x00: Encrypt Login Only, 0x02: No Encryption
                                }
  withSocketsDo $
    flip catches [authErrorHandler,queryErrorHandler] $
    bracket (MSSQL.connect info) MSSQL.close $ \conn -> do
    select_exp1 conn
--    select_exp2 conn
--    select_exp3 conn
--    select_exp4 conn
--    select_exp5 conn
--    select_table1 conn
--    select_table2 conn
--    select_table3 conn
--    select_table4 conn
--    select_table5 conn
--    insert_table1 conn
--    insert_table2 conn
--    update_table1 conn
--    rpc1 conn 3
--    rpc_rv1 conn 3
--    rpc_rv_rs1 conn 3
--    rpc_rs1 conn 5
--    rpc_rs2 conn 3 7
--    rpc_rs3 conn 10
--    rpc_sql1 conn
--    rpc_sql2 conn 5
--    rpc_sql3 conn 5 13
--    rpc_insert1 conn "title1" "content1"
--    rpc_insert2 conn "title2" "content2"
--    test_select1 conn
--    test_select2 conn
--    test_select3 conn
--    test_select4 conn
    return ()

  where    

    authErrorHandler :: Handler ()
    authErrorHandler = Handler f
      where
        f :: AuthError -> IO ()
        f (AuthError (Info number state class' msgText serverName procName lineNumber)) = do
          putStrLn $ "number: " <> (show number)
          putStrLn $ "state: "  <> (show state)
          putStrLn $ "class: "  <> (show class')
          T.putStrLn $ "msgText: " <> msgText
          T.putStrLn $ "serverName: " <> serverName
          T.putStrLn $ "procName: " <> procName
          putStrLn $ "lineNumber: " <> (show lineNumber)

    queryErrorHandler :: Handler ()
    queryErrorHandler = Handler f
      where
        f :: QueryError -> IO ()
        f (QueryError (Info number state class' msgText serverName procName lineNumber)) = do
          putStrLn $ "number: " <> (show number)
          putStrLn $ "state: "  <> (show state)
          putStrLn $ "class: "  <> (show class')
          T.putStrLn $ "msgText: " <> msgText
          T.putStrLn $ "serverName: " <> serverName
          T.putStrLn $ "procName: " <> procName
          putStrLn $ "lineNumber: " <> (show lineNumber)



select_exp1 :: Connection -> IO ()
select_exp1 conn = do
  [Only num] <- MSSQL.sql conn "SELECT 111" :: IO [Only Int]
  putStrLn $ show num


  
select_exp2 :: Connection -> IO ()
select_exp2 conn = do
  [Only mnum] <- MSSQL.sql conn "SELECT 100 + 11" :: IO [Only (Maybe Int)]
  case mnum of
    Nothing -> putStrLn "error: Uncomputable expression"
    Just num -> putStrLn $ show num



select_exp3 :: Connection -> IO ()
select_exp3 conn = do
  [Only mnum] <- MSSQL.sql conn "SELECT 100 / 0" :: IO [Only (Maybe Int)]
  case mnum of
    Nothing -> putStrLn "error: Uncomputable expression"
    Just num -> putStrLn $ show num



select_exp4 :: Connection -> IO ()
select_exp4 conn = do
  [(mnum1,mnum2)] <- MSSQL.sql conn $ "SELECT LEN(N'A" <> T.replicate 1900 "N" <> "Z')," <> "LEN(N'A" <> T.replicate 3000 "N" <> "Z')" :: IO [(Maybe Int, Maybe Int)]
  case (mnum1,mnum2) of
    (Nothing,_) -> putStrLn "error: Uncomputable expression"
    (_,Nothing) -> putStrLn "error: Uncomputable expression"
    (Just num1,Just num2) -> putStrLn $ show num1 <> ":" <> show num2



select_exp5 :: Connection -> IO ()
select_exp5 conn = do
  [(text1,text2)] <- MSSQL.sql conn $ "SELECT N'A" <> T.replicate 1900 "N" <> "Z'," <> "N'A" <> T.replicate 3000 "N" <> "Z'" :: IO [(T.Text,T.Text)]
  T.putStrLn text1
  T.putStrLn text2


  
select_table1 :: Connection -> IO ()
select_table1 conn = do
  rs <- MSSQL.sql conn "SELECT * FROM TSome" :: IO [(Int,T.Text,T.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)]
  forM_ rs $ \(id,title,content,price,created,modified,deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


  
select_table2 :: Connection -> IO ()
select_table2 conn = do
  rs <- MSSQL.sql conn "SELECT * FROM TSome"
  f rs
  where
    f :: [(Int,T.Text,T.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)] -> IO ()
    f rs = forM_ rs $ \(id,title,content,price,created,modified,deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""



select_table3 :: Connection -> IO ()
select_table3 conn = do
  [Only num] <- MSSQL.sql conn "SELECT COUNT (*) FROM TSome" :: IO [Only Int]
  putStrLn $ show num

  
      
data Some = Some { someID :: Int
                 , someTitle :: T.Text
                 , someContent :: T.Text
                 , somePrice :: Money
                 , someCreated:: UTCTime
                 , someModified:: Maybe UTCTime
                 , someDeleted:: Maybe UTCTime
                 }
          deriving (Show)

instance MSSQL.Row Some where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7] [b1,b2,b3,b4,b5,b6,b7] = Some d1 d2 d3 d4 d5 d6 d7
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      
      mcdTypeInfo :: MetaColumnData -> TypeInfo
      mcdTypeInfo (MetaColumnData _ _ ti _ _) = ti
      
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 7"

select_table4 :: Connection -> IO ()
select_table4 conn = do
  rs <- MSSQL.sql conn "SELECT * FROM TSome" :: IO [Some]
  mapM_ f rs
  where
    f :: Some -> IO ()
    f (Some id title content price created modified deleted) = do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


  
select_table5 :: Connection -> IO ()
select_table5 conn = do
  (rs1,rs2) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 ORDER BY someID DESC" :: IO ([Some],[Some])
  mapM_ f rs1
  putStrLn "----"
  mapM_ f rs2
  where
    f :: Some -> IO ()
    f (Some id title content price created modified deleted) = do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


      
insert_table1 :: Connection -> IO ()
insert_table1 conn = do
  _ <- MSSQL.sql conn "INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES(N'title',N'content',12345.60,GETDATE())" :: IO ()
  return ()


insert_table2 :: Connection -> IO ()
insert_table2 conn = do
  _ <- MSSQL.sql conn "INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES(N'title',N'content',23456.70,GETDATE())" :: IO ()
  return ()

  
update_table1 :: Connection -> IO ()
update_table1 conn = do
  _ <- MSSQL.sql conn "UPDATE TSome SET somePrice = somePrice + 1 WHERE someID = 5" :: IO ()
  return ()






rpc1 :: Connection -> Int -> IO ()
rpc1 conn val1 = do
  RpcResult rets () () <- MSSQL.rpc conn $
    RpcQuery ("SP_Input1"::T.Text) $ RpcParamVal "@Val1" TIIntN4 val1
  putStrLn $ "rets: " <> (show rets)


rpc_rv1 :: Connection -> Int -> IO ()
rpc_rv1 conn id = do
  RpcResult rets (Only id') () <- MSSQL.rpc conn $
    RpcQuery ("SP_OutputTwice"::T.Text) $ RpcParamRef "@ID" TIIntN4 id
    :: IO (RpcResult (Only Int) ())
  putStrLn $ "rets: " <> (show rets)
  putStrLn $ "id: " <> (show id')


rpc_rv_rs1 :: Connection -> Int -> IO ()
rpc_rv_rs1 conn id = do
  RpcResult rets (Only id') rs <- MSSQL.rpc conn $
    RpcQuery ("SP_OutputTwice_SelectSome"::T.Text) $ RpcParamRef "@ID" TIIntN4 id
    :: IO (RpcResult (Only Int) [Some])
  putStrLn $ "rets: " <> (show rets)
  putStrLn $ "id: " <> (show id')
  mapM_ f rs
  where
    f :: Some -> IO ()
    f (Some id title content price created modified deleted) = do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""



rpc_rs1 :: Connection -> Int -> IO ()
rpc_rs1 conn id = do
  RpcResult rets () rs <- MSSQL.rpc conn $
    RpcQuery ("SP_SelectSomeByID"::T.Text) $ RpcParamVal "@ID" TIIntN4 id
  putStrLn $ "rets: " <> (show rets)
  f rs
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""

      

rpc_rs2 :: Connection -> Int -> Int -> IO ()
rpc_rs2 conn id1 id2 = do
  (RpcResult rets1 () rs1, RpcResult rets2 () rs2) <- MSSQL.rpc conn
    ( RpcQuery ("SP_SelectSomeByID"::T.Text) $ RpcParamVal "@ID" TIIntN4 id1
    , RpcQuery ("SP_SelectSomeByID"::T.Text) $ RpcParamVal "@ID" TIIntN4 id2
    )
  putStrLn $ "rets1: " <> (show rets1)
  f rs1
  putStrLn "----"
  putStrLn $ "rets2: " <> (show rets2)
  f rs2
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


rpc_rs3 :: Connection -> Int -> IO ()
rpc_rs3 conn id = do
  RpcResult rets () (rs1,rs2) <- MSSQL.rpc conn $
    RpcQuery ("SP_SplitSomeByID"::T.Text) $ RpcParamVal "" TIIntN4 id
  putStrLn $ "rets: " <> (show rets)
  f rs1
  putStrLn "----"
  f rs2
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""




rpc_sql1 :: Connection -> IO ()
rpc_sql1 conn = do
  RpcResult rets () rs <- MSSQL.rpc conn $
    RpcQuery SP_ExecuteSql $ nvarcharVal "" "SELECT * FROM TSome"
  putStrLn $ "rets: " <> (show rets)
  f rs
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


rpc_sql2 :: Connection -> Int -> IO ()
rpc_sql2 conn max = do
  RpcResult rets () rs <- MSSQL.rpc conn $
    RpcQuery ("sp_executesql"::T.Text) ( nvarcharVal "" "SELECT * FROM TSome WHERE someID < @Max"
                                       , nvarcharVal "" "@Max Int"
                                       , RpcParamVal "" TIIntN4 max
                                       )
  putStrLn $ "rets: " <> (show rets)
  f rs
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""


rpc_sql3 :: Connection -> Int -> Int -> IO ()
rpc_sql3 conn min max = do
  RpcResult rets () rs <- MSSQL.rpc conn $
    RpcQuery (0xa::Word16) ( nvarcharVal "" "SELECT * FROM TSome WHERE @Min < someID AND someID < @Max"
                           , nvarcharVal "" "@Min Int, @Max Int"
                           , RpcParamVal "@Min" TIIntN4 min
                           , RpcParamVal "@Max" TIIntN4 max
                           )
  putStrLn $ "rets: " <> (show rets)
  f rs
  where
    f :: [Some] -> IO ()
    f rs = forM_ rs $ \(Some id title content price created modified deleted) -> do
      putStr   $ (show id)
      T.putStr $ ", " <> title
      T.putStr $ ", " <> content
      putStr   $ ", " <> (show price)
      putStr   $ ", " <> (show created)
      putStr   $ ", " <> (show modified)
      putStr   $ ", " <> (show deleted)
      putStrLn ""




rpc_insert1 :: Connection -> T.Text -> T.Text -> IO ()
rpc_insert1 conn title content = do
  RpcResult rets (Only id) () <- MSSQL.rpc conn $
    RpcQuery ("SP_InsertSome"::T.Text) ( nvarcharVal "@Title" title
                                       , nvarcharVal "@Content" content
                                       , RpcParamRef "@ID" TIIntN4 (Nothing :: Maybe Int)
                                       )
    :: IO (RpcResult (Only Int) ())
  putStrLn $ "rets: " <> (show rets)
  putStrLn $ "id: " <> (show id)


rpc_insert2 :: Connection -> T.Text -> T.Text -> IO ()
rpc_insert2 conn title content = do
  RpcResult rets (id,time) () <- MSSQL.rpc conn $
    RpcQuery ("SP_InsertSomeDate"::T.Text) ( nvarcharVal "@Title" title
                                           , nvarcharVal "@Content" content
                                           , RpcParamRef "@ID" TIIntN4 (Nothing :: Maybe Int)
                                           , RpcParamRef "@Created" TIDateTimeN8 (Nothing :: Maybe UTCTime)
                                           )
    :: IO (RpcResult (Int,UTCTime) ())
  putStrLn $ "rets: " <> (show rets)
  putStrLn $ "id: " <> (show id)
  putStrLn $ "time: " <> (show time)





test_select1 :: Connection -> IO ()
test_select1 conn = do
  rs <- MSSQL.sql conn "SELECT t1ID, t1Bit, t1Int1, t1Int2, t1Int4, t1Int8, t1Money4, t1Money8, t1DateTime4, t1DateTime8, t1Flt4, t1Flt8, t1BitN, t1IntN1, t1IntN2, t1IntN4, t1IntN8, t1MoneyN4, t1MoneyN8, t1DateTimeN4, t1DateTimeN8, t1FltN4, t1FltN8 FROM TTypes1"
  f rs
  where
    f :: [(Int,Bool,Int,Int,Int,Integer,Money,Money,UTCTime,UTCTime,Float,Double,(Maybe Bool),(Maybe Int),(Maybe Int),(Maybe Int),(Maybe Integer),(Maybe Money),(Maybe Money),(Maybe UTCTime),(Maybe UTCTime),(Maybe Float),(Maybe Double))] -> IO ()
    f rs = forM_ rs $ \(id,bit,int1,int2,int4,int8,money4,money8,dt4,dt8,flt4,flt8,bitn,intn1,intn2,intn4,intn8,moneyn4,moneyn8,dtn4,dtn8,fltn4,fltn8) -> do
      putStr $ (show id)
      putStr $ ", " <> (show int1)
      putStr $ ", " <> (show int2)
      putStr $ ", " <> (show int4)
      putStr $ ", " <> (show int8)
      putStr $ ", " <> (show money4)
      putStr $ ", " <> (show money8)
      putStr $ ", " <> (show dt4)
      putStr $ ", " <> (show dt8)
      putStr $ ", " <> (show flt4)
      putStr $ ", " <> (show flt8)
      putStrLn ""
      putStr $ "\t" <> (show intn1)
      putStr $ ", " <> (show intn2)
      putStr $ ", " <> (show intn4)
      putStr $ ", " <> (show intn8)
      putStr $ ", " <> (show moneyn4)
      putStr $ ", " <> (show moneyn8)
      putStr $ ", " <> (show dtn4)
      putStr $ ", " <> (show dtn8)
      putStr $ ", " <> (show fltn4)
      putStr $ ", " <> (show fltn8)
      putStrLn ""


test_select2 :: Connection -> IO ()
test_select2 conn = do
  rs <- MSSQL.sql conn "SELECT t2ID, t2GUID, t2Decimal0, t2Decimal1, t2Decimal37, t2Decimal38, t2Numeric0, t2Numeric1, t2Numeric37, t2Numeric38, t2GUIDN, t2DecimalN0, t2DecimalN1, t2DecimalN37, t2DecimalN38, t2NumericN0, t2NumericN1, t2NumericN37, t2NumericN38 FROM TTypes2"
  f rs
  where
    f :: [(Int,UUID,Decimal,Decimal,Decimal,Decimal,Decimal,Decimal,Decimal,Decimal,(Maybe UUID),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal),(Maybe Decimal))] -> IO ()
    f rs = forM_ rs $ \(id,guid,dec0,dec1,dec37,dec38,num0,num1,num37,num38,guidn,decn0,decn1,decn37,decn38,numn0,numn1,numn37,numn38) -> do
      putStr $ (show id)
      putStr $ ", " <> (show guid)
      putStr $ ", " <> (show dec0)
      putStr $ ", " <> (show dec1)
      putStr $ ", " <> (show dec37)
      putStr $ ", " <> (show dec38)
      putStr $ ", " <> (show num0)
      putStr $ ", " <> (show num1)
      putStr $ ", " <> (show num37)
      putStr $ ", " <> (show num38)
      putStrLn ""
      putStr $ "\t" <> (show guidn)
      putStr $ ", " <> (show decn0)
      putStr $ ", " <> (show decn1)
      putStr $ ", " <> (show decn37)
      putStr $ ", " <> (show decn38)
      putStr $ ", " <> (show numn0)
      putStr $ ", " <> (show numn1)
      putStr $ ", " <> (show numn37)
      putStr $ ", " <> (show numn38)
      putStrLn ""


test_select3 :: Connection -> IO ()
test_select3 conn = do
  rs <- MSSQL.sql conn "SELECT t3ID, t3BigChar, t3BigVarChar, t3Text, t3NChar, t3NVarChar, t3NText, t3BigBinary, t3BigVarBinary, t3Image, t3BigCharN, t3BigVarCharN, t3TextN, t3NCharN, t3NVarCharN, t3NTextN, t3BigBinaryN, t3BigVarBinaryN, t3ImageN FROM TTypes3"
  f rs
  where
    f :: [(Int,B.ByteString,B.ByteString,B.ByteString,T.Text,T.Text,T.Text,B.ByteString,B.ByteString,B.ByteString,(Maybe B.ByteString),(Maybe B.ByteString),(Maybe B.ByteString),(Maybe T.Text),(Maybe T.Text),(Maybe T.Text),(Maybe B.ByteString),(Maybe B.ByteString),(Maybe B.ByteString))] -> IO ()
    f rs = forM_ rs $ \(id,char,vchar,text,nchar,nvchar,ntext,binary,vbinary,image,charn,vcharn,textn,ncharn,nvcharn,ntextn,binaryn,vbinaryn,imagen) -> do
      putStr $ (show id)
      putStr $ ", " <> (show char)
      putStr $ ", " <> (show vchar)
      putStr $ ", " <> (show text)
      T.putStr $ ", " <> nchar
      T.putStr $ ", " <> nvchar
      T.putStr $ ", " <> ntext
      putStr $ ", " <> (show binary)
      putStr $ ", " <> (show vbinary)
      putStr $ ", " <> (show image)
      putStrLn ""
      putStr $ "\t" <> (show charn)
      putStr $ ", " <> (show vcharn)
      putStr $ ", " <> (show textn)
      T.putStr $ ", " <> case ncharn of
                           Nothing -> "Nothing"
                           Just t -> t
      T.putStr $ ", " <> case nvcharn of
                           Nothing -> "Nothing"
                           Just t -> t
      T.putStr $ ", " <> case ntextn of
                           Nothing -> "Nothing"
                           Just t -> t
      putStr $ ", " <> (show binaryn)
      putStr $ ", " <> (show vbinaryn)
      putStr $ ", " <> (show imagen)
      putStrLn ""


test_select4 :: Connection -> IO ()
test_select4 conn = do
  rs <- MSSQL.sql conn "SELECT t3ID, t3BigChar, t3BigVarChar, t3Text, t3NChar, t3NVarChar, t3NText, t3BigBinary, t3BigVarBinary, t3Image, t3BigCharN, t3BigVarCharN, t3TextN, t3NCharN, t3NVarCharN, t3NTextN, t3BigBinaryN, t3BigVarBinaryN, t3ImageN FROM TTypes3"
  f rs
  where
    f :: [(Int,LB.ByteString,LB.ByteString,LB.ByteString,LT.Text,LT.Text,LT.Text,LB.ByteString,LB.ByteString,LB.ByteString,(Maybe LB.ByteString),(Maybe LB.ByteString),(Maybe LB.ByteString),(Maybe LT.Text),(Maybe LT.Text),(Maybe LT.Text),(Maybe LB.ByteString),(Maybe LB.ByteString),(Maybe LB.ByteString))] -> IO ()
    f rs = forM_ rs $ \(id,char,vchar,text,nchar,nvchar,ntext,binary,vbinary,image,charn,vcharn,textn,ncharn,nvcharn,ntextn,binaryn,vbinaryn,imagen) -> do
      putStr $ (show id)
      putStr $ ", " <> (show char)
      putStr $ ", " <> (show vchar)
      putStr $ ", " <> (show text)
      LT.putStr $ ", " <> nchar
      LT.putStr $ ", " <> nvchar
      LT.putStr $ ", " <> ntext
      putStr $ ", " <> (show binary)
      putStr $ ", " <> (show vbinary)
      putStr $ ", " <> (show image)
      putStrLn ""
      putStr $ "\t" <> (show charn)
      putStr $ ", " <> (show vcharn)
      putStr $ ", " <> (show textn)
      LT.putStr $ ", " <> case ncharn of
                           Nothing -> "Nothing"
                           Just t -> t
      LT.putStr $ ", " <> case nvcharn of
                           Nothing -> "Nothing"
                           Just t -> t
      LT.putStr $ ", " <> case ntextn of
                           Nothing -> "Nothing"
                           Just t -> t
      putStr $ ", " <> (show binaryn)
      putStr $ ", " <> (show vbinaryn)
      putStr $ ", " <> (show imagen)
      putStrLn ""

