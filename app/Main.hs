{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Database.Tds.Primitives.Fixed
import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Connection as MSSQL
import qualified Database.MSSQLServer.Query as MSSQL

import Data.Time (UTCTime(..))
import Data.Time (getCurrentTime,addUTCTime)
import Data.UUID.Types (UUID(..))
import qualified Data.UUID.Types as UUID
import Data.Fixed (Fixed(..))
import Data.Word (Word16)

import Control.Monad (forM_)
import Text.RawString.QQ (r)

main :: IO ()
main = do
  let info = defaultConnectInfo { connectHost = "localhost"
                                , connectPort = "1433"
                                , connectDatabase = "mss-test"
                                , connectUser = "mss-test_admin"
                                , connectPassword = "msst_adm-1234"
                                , connectEncryption = 0x02 -- [MEMO] 0x00: Encrypt Login Only, 0x02: No Encryption
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
--    select_table6 conn
--    select_table7 conn
--    select_table8 conn
--    select_table9 conn
--    insert_table1 conn
--    insert_table2 conn
--    insert_table3 conn
--    insert_table4 conn
--    update_table1 conn
--    delete_table1 conn
--    create_proc1 conn
--    create_proc2 conn
--    create_proc3 conn
--    create_proc4 conn
--    create_proc5 conn
--    exec_proc1_1 conn
--    exec_proc1_2 conn
--    exec_proc1_3 conn
--    exec_proc1_4 conn
--    exec_proc2_1 conn
--    exec_proc2_2 conn
--    exec_proc3_1 conn
--    exec_proc3_2 conn
--    exec_proc2_1' conn
--    exec_proc2_2' conn
--    exec_proc3_1' conn
--    exec_proc3_2' conn
--    rpc1 conn 3
--    rpc2 conn 3
--    rpc3 conn
--    rpc4 conn
--    rpc4' conn
--    rpc5 conn
--    rpc5' conn
--    rpc_rv1 conn 3
--    rpc_rv_rs1 conn 3
--    rpc_rs1 conn 5
--    rpc_rs2 conn 3 7
--    rpc_rs3 conn 10
--    rpc_sql1 conn
--    rpc_sql2 conn 5
--    rpc_sql3 conn 5 13
--    rpc_sql4 conn
--    rpc_insert1 conn "title1" "content1"
--    rpc_insert2 conn "title2" "content2"
--    test_select1 conn
--    test_select2 conn
--    test_select3 conn
--    test_select4 conn
--    test_transaction1 conn
--    test_transaction2 conn
    return ()

  where    

    authErrorHandler :: Handler ()
    authErrorHandler = Handler f
      where
        f :: AuthError -> IO ()
        f (AuthError info) = printInfo info

    queryErrorHandler :: Handler ()
    queryErrorHandler = Handler f
      where
        f :: QueryError -> IO ()
        f (QueryError info) = printInfo info


printInfo :: Info -> IO ()
printInfo (Info number state class' msgText serverName procName lineNumber) = do
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
  [(text1,text2)] <- MSSQL.sql conn $ "SELECT N'A" <> T.replicate 1900 "N" <> "Z'," <> "N'A" <> T.replicate 3000 "N" <> "Z'" :: IO [(LT.Text,LT.Text)]
  LT.putStrLn text1
  LT.putStrLn text2


  
select_table1 :: Connection -> IO ()
select_table1 conn = do
  rs <- MSSQL.sql conn "SELECT TOP 10 * FROM TSome" :: IO [(Int,T.Text,T.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)]
  forM_ rs print


  
select_table2 :: Connection -> IO ()
select_table2 conn = do
  rs <- MSSQL.sql conn "SELECT TOP 10 * FROM TSome ORDER BY somePrice, someID"
  f rs
  where
    f :: [(Int,String,LT.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)] -> IO ()
    f rs = forM_ rs print



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
  rs <- MSSQL.sql conn "SELECT TOP 10 * FROM TSome ORDER BY somePrice DESC" :: IO [Some]
  mapM_ print rs


  
select_table5 :: Connection -> IO ()
select_table5 conn = do
  (rs1,rs2) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 AND someID < 12 ORDER BY someID DESC" :: IO ([Some],[Some])
  mapM_ print rs1
  putStrLn "----"
  mapM_ print rs2


  
select_table6 :: Connection -> IO ()
select_table6 conn = do
  (_,rs2) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 AND someID < 12 ORDER BY someID DESC" :: IO ((),[Some])
  mapM_ print rs2


  
select_table7 :: Connection -> IO ()
select_table7 conn = do
  (RowCount rc1,rs2) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 AND someID < 12 ORDER BY someID DESC" :: IO (RowCount,[Some])
  putStrLn $ show rc1
  mapM_ print rs2



select_table8 :: Connection -> IO ()
select_table8 conn = do
  (rs1,_) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 AND someID < 12 ORDER BY someID DESC" :: IO ([Some],())
  mapM_ print rs1


select_table9 :: Connection -> IO ()
select_table9 conn = do
  (rs1,RowCount rc2) <- MSSQL.sql conn "SELECT * FROM TSome WHERE someID < 8 ORDER BY someID; SELECT * FROM TSome WHERE someID > 8 AND someID < 12 ORDER BY someID DESC" :: IO ([Some],RowCount)
  mapM_ print rs1
  putStrLn "----"
  putStrLn $ show rc2



      
insert_table1 :: Connection -> IO ()
insert_table1 conn = do
  _ <- MSSQL.sql conn "INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES(N'title',N'content',12345.60,GETDATE())" :: IO ()
  return ()


insert_table2 :: Connection -> IO ()
insert_table2 conn = do
  RowCount rc <- MSSQL.sql conn "INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES(N'title',N'content',12345.60,GETDATE())" :: IO RowCount
  putStrLn $ show rc
  return ()


insert_table3 :: Connection -> IO ()
insert_table3 conn = do
  (RowCount rc1,RowCount rc2) <- MSSQL.sql conn "INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES(N'title',N'content',23456.70,GETDATE())" :: IO (RowCount,RowCount)
  putStrLn $ show rc2 <> " " <> show rc2
  return ()

  
insert_table4 :: Connection -> IO ()
insert_table4 conn = do
  () <- MSSQL.sql conn [r|
BEGIN TRAN;
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
COMMIT TRAN;
  |]
  return ()

update_table1 :: Connection -> IO ()
update_table1 conn = do
  RowCount rc <- MSSQL.sql conn "UPDATE TSome SET somePrice = somePrice + 1 WHERE someID < 5" :: IO RowCount
  putStrLn $ show rc
  return ()


delete_table1 :: Connection -> IO ()
delete_table1 conn = do
  RowCount rc <- MSSQL.sql conn "DELETE TSome WHERE someID > 50" :: IO RowCount
  putStrLn $ show rc
  return ()


create_proc1 :: Connection -> IO ()
create_proc1 conn = do
  () <- MSSQL.sql conn "DROP PROCEDURE SP_InputDefault1"
  () <- MSSQL.sql conn [r|
CREATE PROCEDURE SP_InputDefault1
	@Val1 int = 11
AS
BEGIN
	RETURN @Val1 + 100
END
  |]
  return ()


create_proc2 :: Connection -> IO ()
create_proc2 conn = do
  () <- MSSQL.sql conn "DROP PROCEDURE SP_OutputDefault1"
  () <- MSSQL.sql conn [r|
CREATE PROCEDURE SP_OutputDefault1
	@Ref1 int = 12 OUTPUT
AS
BEGIN
	SET @Ref1 = @Ref1 + 33
	Return 0
END
  |]
  return ()


create_proc3 :: Connection -> IO ()
create_proc3 conn = do
  () <- MSSQL.sql conn "DROP PROCEDURE SP_InputOutput1"
  () <- MSSQL.sql conn [r|
CREATE PROCEDURE SP_InputOutput1
	 @vBit bit
	,@vTinyint tinyint
	,@vSmallInt smallint
	,@vInt int
	,@vBigint bigint
	,@vSmallmoney smallmoney
	,@vMoney money
	,@vSmallDatetime smalldatetime
	,@vDatetime datetime
	,@vFloat24 float(24)
	,@vReal real
	,@vFloat53 float(53)
	,@vDouble double precision
	,@vUniqueidentifier uniqueidentifier
	,@vDecimal decimal(10,4)
	,@vNumeric numeric(11,5)
	,@vChar char(20)
	,@vVarchar varchar(20)
	,@vText text
	,@vNchar nchar(20)
	,@vNvarchar nvarchar(20)
	,@vNtext ntext
	,@vBinary binary(20)
	,@vVarbinary varbinary(20)
	,@vImage image

	,@rBit bit OUTPUT
	,@rTinyint tinyint OUTPUT
	,@rSmallInt smallint OUTPUT
	,@rInt int OUTPUT
	,@rBigint bigint OUTPUT
	,@rSmallmoney smallmoney OUTPUT
	,@rMoney money OUTPUT
	,@rSmallDatetime smalldatetime OUTPUT
	,@rDatetime datetime OUTPUT
	,@rFloat24 float(24) OUTPUT
	,@rReal real OUTPUT
	,@rFloat53 float(53) OUTPUT
	,@rDouble double precision OUTPUT
	,@rUniqueidentifier uniqueidentifier OUTPUT
	,@rDecimal decimal(10,4) OUTPUT
	,@rNumeric numeric(11,5) OUTPUT
	,@rChar char(20) OUTPUT
	,@rVarchar varchar(20) OUTPUT
--	,@rText text OUTPUT
	,@rNchar nchar(20) OUTPUT
	,@rNvarchar nvarchar(20) OUTPUT
--	,@rNtext ntext OUTPUT
	,@rBinary binary(20) OUTPUT
	,@rVarbinary varbinary(20) OUTPUT
--	,@rImage image OUTPUT

AS
BEGIN

	SET @rBit = @vBit
	SET @rTinyint = @vTinyint
	SET @rSmallInt = @vSmallInt
	SET @rInt = @vInt
	SET @rBigint = @vBigint
	SET @rSmallmoney = @vSmallmoney
	SET @rMoney = @vMoney
	SET @rSmallDatetime = @vSmallDatetime
	SET @rDatetime = @vDatetime
	SET @rFloat24 = @vFloat24
	SET @rReal = @vReal
	SET @rFloat53 = @vFloat53
	SET @rDouble = @vDouble
	SET @rUniqueidentifier = @vUniqueidentifier
	SET @rDecimal = @vDecimal
	SET @rNumeric = @vNumeric
	SET @rChar = @vChar
	SET @rVarchar = @vVarchar
--	SET @rText = @vText
	SET @rNchar = @vNchar
	SET @rNvarchar = @vNvarchar
--	SET @rNtext = @vNtext
	SET @rBinary = @vBinary
	SET @rVarbinary = @vVarbinary
--	SET @rImage = @vImage

	Return 0
END
  |]
  return ()


create_proc4 :: Connection -> IO ()
create_proc4 conn = do
  () <- MSSQL.sql conn "DROP PROCEDURE SP_InputOutputDefault1"
  () <- MSSQL.sql conn [r|
CREATE PROCEDURE SP_InputOutputDefault1
	 @vBit bit = 1
	,@vTinyint tinyint = 21
	,@vSmallInt smallint = 22
	,@vInt int = 23
	,@vBigint bigint = 24
	,@vSmallmoney smallmoney = 2345.6789
	,@vMoney money = 3345.6789
	,@vSmallDatetime smalldatetime = '2019/08/01 10:00'
	,@vDatetime datetime = '2019/08/01 10:30'
	,@vFloat24 float(24) = 4345.6789
	,@vReal real = 5345.6789
	,@vFloat53 float(53) = 6345.6789
	,@vDouble double precision = 6345.6789
	,@vUniqueidentifier uniqueidentifier = '22223333-4444-aaaa-bbbb-ccccddddffff'
	,@vDecimal decimal(10,4) = 7345.6789
	,@vNumeric numeric(11,5) = 8345.67891
	,@vChar char(20) = 'efgh1234'
	,@vVarchar varchar(20) = 'efgh1234'
	,@vText text = 'efgh1234'
	,@vNchar nchar(20) = N'efgh1234'
	,@vNvarchar nvarchar(20) = N'efgh1234'
	,@vNtext ntext = 'efgh1234'
	,@vBinary binary(20) = 0x0102030405060708
	,@vVarbinary varbinary(20) = 0x0102030405060708
	,@vImage image = 0x0102030405060708

	,@rBit bit OUTPUT
	,@rTinyint tinyint OUTPUT
	,@rSmallInt smallint OUTPUT
	,@rInt int OUTPUT
	,@rBigint bigint OUTPUT
	,@rSmallmoney smallmoney OUTPUT
	,@rMoney money OUTPUT
	,@rSmallDatetime smalldatetime OUTPUT
	,@rDatetime datetime OUTPUT
	,@rFloat24 float(24) OUTPUT
	,@rReal real OUTPUT
	,@rFloat53 float(53) OUTPUT
	,@rDouble double precision OUTPUT
	,@rUniqueidentifier uniqueidentifier OUTPUT
	,@rDecimal decimal(10,4) OUTPUT
	,@rNumeric numeric(11,5) OUTPUT
	,@rChar char(20) OUTPUT
	,@rVarchar varchar(20) OUTPUT
--	,@rText text OUTPUT
	,@rNchar nchar(20) OUTPUT
	,@rNvarchar nvarchar(20) OUTPUT
--	,@rNtext ntext OUTPUT
	,@rBinary binary(20) OUTPUT
	,@rVarbinary varbinary(20) OUTPUT
--	,@rImage image OUTPUT

AS
BEGIN

	SET @rBit = @vBit
	SET @rTinyint = @vTinyint
	SET @rSmallInt = @vSmallInt
	SET @rInt = @vInt
	SET @rBigint = @vBigint
	SET @rSmallmoney = @vSmallmoney
	SET @rMoney = @vMoney
	SET @rSmallDatetime = @vSmallDatetime
	SET @rDatetime = @vDatetime
	SET @rFloat24 = @vFloat24
	SET @rReal = @vReal
	SET @rFloat53 = @vFloat53
	SET @rDouble= @vDouble
	SET @rUniqueidentifier = @vUniqueidentifier
	SET @rDecimal = @vDecimal
	SET @rNumeric = @vNumeric
	SET @rChar = @vChar
	SET @rVarchar = @vVarchar
--	SET @rText = @vText
	SET @rNchar = @vNchar
	SET @rNvarchar = @vNvarchar
--	SET @rNtext = @vNtext
	SET @rBinary = @vBinary
	SET @rVarbinary = @vVarbinary
--	SET @rImage = @vImage

	Return 0
END
  |]
  return ()


create_proc5 :: Connection -> IO ()
create_proc5 conn = do
  () <- MSSQL.sql conn "DROP PROCEDURE SP_InputOutputDefault2"
  () <- MSSQL.sql conn [r|
CREATE PROCEDURE SP_InputOutputDefault2
	 @rBit bit = 1 OUT
	,@rTinyint tinyint = 21 OUT
	,@rSmallInt smallint = 22 OUT
	,@rInt int = 23 OUT
	,@rBigint bigint = 24 OUT
	,@rSmallmoney smallmoney = 2345.6789 OUT
	,@rMoney money = 3345.6789 OUT
	,@rSmallDatetime smalldatetime = '2019/08/01 10:00' OUT
	,@rDatetime datetime = '2019/08/01 10:30' OUT
	,@rFloat24 float(24) = 4345.6789 OUT
	,@rReal real = 5345.6789 OUT
	,@rFloat53 float(53) = 6345.6789 OUT
	,@rDouble double precision = 6345.6789 OUT
	,@rUniqueidentifier uniqueidentifier = '22223333-4444-aaaa-bbbb-ccccddddffff' OUT
	,@rDecimal decimal(10,4) = 7345.6789 OUT
	,@rNumeric numeric(11,5) = 8345.67891 OUT
	,@rChar char(20) = 'efgh1234' OUT
	,@rVarchar varchar(20) = 'efgh1234' OUT
--	,@rText text = 'efgh1234' OUT
	,@rNchar nchar(20) = N'efgh1234' OUT
	,@rNvarchar nvarchar(20) = N'efgh1234' OUT
--	,@rNtext ntext = 'efgh1234' OUT
	,@rBinary binary(20) = 0x0102030405060708 OUT
	,@rVarbinary varbinary(20) = 0x0102030405060708 OUT
--	,@rImage image = 0x0102030405060708 OUT

AS
BEGIN
	Return 0
END
  |]
  return ()


exec_proc1_1 :: Connection -> IO ()
exec_proc1_1 conn = do
  _ <- MSSQL.sql conn "EXEC SP_Input1 @Val1=3" :: IO ()
  return ()

exec_proc1_2 :: Connection -> IO ()
exec_proc1_2 conn = do
  _ <- MSSQL.sql conn "EXEC SP_Input1 @Val1=3;EXEC SP_Input1 @Val1=4" :: IO ((),())
  return ()

exec_proc1_3 :: Connection -> IO ()
exec_proc1_3 conn = do
  ReturnStatus rets <- MSSQL.sql conn "EXEC SP_Input1 @Val1=3" :: IO ReturnStatus
  putStrLn $ show rets
  return ()

exec_proc1_4 :: Connection -> IO ()
exec_proc1_4 conn = do
  (ReturnStatus rets1, ReturnStatus rets2) <- MSSQL.sql conn "EXEC SP_Input1 @Val1=3;EXEC SP_Input1 @Val1=4" :: IO (ReturnStatus,ReturnStatus)
  putStrLn $ show rets1 <> " " <> show rets2
  return ()

exec_proc2_1 :: Connection -> IO ()
exec_proc2_1 conn = do
  _ <- MSSQL.sql conn "EXEC SP_OutputTwice @ID=3" :: IO ()
  return ()

exec_proc2_2 :: Connection -> IO ()
exec_proc2_2 conn = do
  _ <- MSSQL.sql conn "EXEC SP_OutputTwice @ID=3;EXEC SP_OutputTwice @ID=4" :: IO ((),())
  return ()

exec_proc3_1 :: Connection -> IO ()
exec_proc3_1 conn = do
  _ <- MSSQL.sql conn "EXEC SP_OutputTwice_SelectSome @ID=3" :: IO ()
  return ()

exec_proc3_2 :: Connection -> IO ()
exec_proc3_2 conn = do
  _ <- MSSQL.sql conn "EXEC SP_OutputTwice_SelectSome @ID=3;EXEC SP_OutputTwice_SelectSome @ID=4" :: IO ((),())
  return ()


exec_proc2_1' :: Connection -> IO ()
exec_proc2_1' conn = do
  ReturnStatus rs <- MSSQL.sql conn "EXEC SP_OutputTwice @ID=3" :: IO ReturnStatus
  putStrLn $ show rs
  return ()

exec_proc2_2' :: Connection -> IO ()
exec_proc2_2' conn = do
  (ReturnStatus rs1, ReturnStatus rs2) <- MSSQL.sql conn "EXEC SP_OutputTwice @ID=3;EXEC SP_OutputTwice @ID=4" :: IO (ReturnStatus,ReturnStatus)
  putStrLn $ (show rs1) <> " " <> (show rs2)
  return ()

exec_proc3_1' :: Connection -> IO ()
exec_proc3_1' conn = do
  ReturnStatus rs <- MSSQL.sql conn "EXEC SP_OutputTwice_SelectSome @ID=3" :: IO ReturnStatus
  putStrLn $ show rs
  return ()

exec_proc3_2' :: Connection -> IO ()
exec_proc3_2' conn = do
  (ReturnStatus rs1, ReturnStatus rs2) <- MSSQL.sql conn "EXEC SP_OutputTwice_SelectSome @ID=3;EXEC SP_OutputTwice_SelectSome @ID=4" :: IO (ReturnStatus,ReturnStatus)
  putStrLn $ (show rs1) <> " " <> (show rs2)
  return ()






rpc1 :: Connection -> Int -> IO ()
rpc1 conn val1 = do
  resp <- MSSQL.rpc conn $
--    RpcQuery ("SP_InputDefault1"::T.Text) $ intVal "@Val1" (Just val1)
    RpcQuery ("SP_InputDefault1"::T.Text) ()


  case resp of
    RpcResponse rets () () -> putStrLn $ "rets: " <> (show rets)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc2 :: Connection -> Int -> IO ()
rpc2 conn val1 = do
  resp <- MSSQL.rpc conn $
--    RpcQuery ("SP_OutputDefault1"::T.Text) (intDefRef "@Ref1" :: RpcParam (Maybe Int))
    RpcQuery ("SP_OutputDefault1"::T.Text) (intRef "@Ref1" (Just (3 :: Int)))
    :: IO (RpcResponse (Only Int) ())


  case resp of
    RpcResponse rets (Only ref1) () -> do
      putStrLn $ "rets: " <> (show rets)
      putStrLn $ "ref1: " <> (show ref1)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc3 :: Connection -> IO ()
rpc3 conn = do
  ut1 <- getCurrentTime
  let ut2 = addUTCTime (60*5) ut1
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InputOutput1"::T.Text) (bitVal "@vBit" (Just (1::Int))
                                         ,tinyintVal "@vTinyint" (Just (11::Int))
                                         ,smallintVal "@vSmallint" (Just (12::Int))
                                         ,intVal "@vInt" (Just (13::Int))
                                         ,bigintVal "@vBigint" (Just (14::Integer))
                                         ,smallmoneyVal "@vSmallmoney" (Just (Money 1234.5678))
                                         ,moneyVal "@vMoney" (Just (Money 2234.5678))
                                         ,smalldatetimeVal "@vSmalldatetime" (Just ut1)
                                         ,datetimeVal "@vDatetime" (Just ut2)
                                         ,float24Val "@vFloat24" (Just (3234.5678::Float))
                                         ,realVal "@vReal" (Just (4234.5678::Float))
                                         ,float53Val "@vFloat53" (Just (5234.5678::Double))
                                         ,doubleVal "@vDouble" (Just (5234.5678::Double))
                                         ,uniqueidentifierVal "@vUniqueidentifier" (UUID.fromString "11112222-3333-4444-aaaa-bbbbccccffff")
                                         ,decimalVal "@vDecimal" 10 (Right (6234.5678::Fixed4))
                                         ,numericVal "@vNumeric" 11 (Right (7234.56789::Fixed5))
                                         ,charVal "@vChar" (Just ("abcd1234"::B.ByteString))
                                         ,varcharVal "@vVarchar" (Just ("abcd2234"::B.ByteString))
                                         ,textVal "@vText" (Just ("abcd3234"::B.ByteString))
                                         ,ncharVal "@vNchar" (Just ("abcd4234"::T.Text))
                                         ,nvarcharVal "@vNvarchar" (Just ("abcd5234"::T.Text))
                                         ,ntextVal "@vNtext" (Just ("abcd6234"::T.Text))
                                         ,binaryVal "@vBinary" (Just (B.pack [0,1,2,3,4,5,6,7]))
                                         ,varbinaryVal "@vVarbinary" (Just (B.pack [0,1,2,3,4,5,6,7]))
                                         ,imageVal "@vImage" (Just (B.pack [0,1,2,3,4,5,6,7]))

                                         ,bitRef "@rBit" (Nothing :: Maybe Int)
                                         ,tinyintRef "@rTinyint" (Nothing :: Maybe Int)
                                         ,smallintRef "@rSmallint" (Nothing :: Maybe Int)
                                         ,intRef "@rInt" (Nothing :: Maybe Int)
                                         ,bigintRef "@rBigint" (Nothing :: Maybe Integer)
                                         ,smallmoneyRef "@rSmallmoney" (Nothing :: Maybe Money)
                                         ,moneyRef "@rMoney" (Nothing :: Maybe Money)
                                         ,smalldatetimeRef "@rSmalldatetime" (Nothing :: Maybe UTCTime)
                                         ,datetimeRef "@rDatetime" (Nothing :: Maybe UTCTime)
                                         ,float24Ref "@rFloat24" (Nothing :: Maybe Float)
                                         ,realRef "@rReal" (Nothing :: Maybe Float)
                                         ,float53Ref "@rFloat53" (Nothing :: Maybe Double)
                                         ,doubleRef "@rDouble" (Nothing :: Maybe Double)
                                         ,uniqueidentifierRef "@rUniqueidentifier" (Nothing :: Maybe UUID)
                                         ,decimalRef "@rDecimal" 10 ((Left 4)::(Either Scale (Fixed4)))
                                         ,numericRef "@rNumeric" 11 ((Left 5)::(Either Scale (Fixed5)))
                                         ,charRef "@rChar" 20 Nothing
                                         ,varcharRef "@rVarchar" 20 Nothing
--                                         ,textRef "@rText" 20 Nothing
                                         ,ncharRef "@rNchar" 20 Nothing
                                         ,nvarcharRef "@rNvarchar" 20 Nothing
--                                         ,ntextRef "@rNtext" 20 Nothing
                                         ,binaryRef "@rBinary" 20 Nothing
                                         ,varbinaryRef "@rVarbinary" 20 Nothing
--                                         ,imageRef "@rImage" 20 Nothing
                                         )
    :: IO (RpcResponse ( (Maybe Bool)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Integer)
                       , (Maybe Money)
                       , (Maybe Money)
                       , (Maybe UTCTime)
                       , (Maybe UTCTime)
                       , (Maybe Float)
                       , (Maybe Float)
                       , (Maybe Double)
                       , (Maybe Double)
                       , (Maybe UUID)
                       , (Maybe Fixed4)
                       , (Maybe Fixed5)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       , (Maybe T.Text)
                       , (Maybe T.Text)
--                       , (Maybe T.Text)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       ) ())


  case resp of
    RpcResponse rets ((Just rBit)
                     ,(Just rTinyint)
                     ,(Just rSmallint)
                     ,(Just rInt)
                     ,(Just rBigint)
                     ,(Just rSmallmoney)
                     ,(Just rMoney)
                     ,(Just vSmalldatetime)
                     ,(Just vDatetime)
                     ,(Just vFloat24)
                     ,(Just vReal)
                     ,(Just vFloat53)
                     ,(Just vDouble)
                     ,(Just vUniqueidentifier)
                     ,(Just vDecimal)
                     ,(Just vNumeric)
                     ,(Just vChar)
                     ,(Just vVarchar)
--                     ,(Just vText)
                     ,(Just vNchar)
                     ,(Just vNvarchar)
--                     ,(Just vNtext)
                     ,(Just vBinary)
                     ,(Just vVarbinary)
--                     ,(Just vImage)
                     ) () -> do
      putStrLn $ "rets: " <> (show rets)
--      putStrLn $ "ref1: " <> (show ref1)
      putStrLn $ (show rBit)
        <> " " <> (show rTinyint)
        <> " " <> (show rSmallint)
        <> " " <> (show rInt)
        <> " " <> (show rBigint)
        <> " " <> (show rSmallmoney)
        <> " " <> (show rMoney)
        <> " " <> (show vSmalldatetime)
        <> " " <> (show vDatetime)
        <> " " <> (show vFloat24)
        <> " " <> (show vReal)
        <> " " <> (show vFloat53)
        <> " " <> (show vDouble)
        <> " " <> (show vUniqueidentifier)
        <> " " <> (show vDecimal)
        <> " " <> (show vNumeric)
        <> " " <> (show vChar)
        <> " " <> (show vVarchar)
--        <> " " <> (show vText)
        <> " " <> (show vNchar)
        <> " " <> (show vNvarchar)
--        <> " " <> (show vNtext)
        <> " " <> (show vBinary)
        <> " " <> (show vVarbinary)
--        <> " " <> (show vImage)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc4 :: Connection -> IO ()
rpc4 conn = do
  ut1 <- getCurrentTime
  let ut2 = addUTCTime (60*5) ut1
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InputOutputDefault1"::T.Text) (bitVal "@vBit" (Just (1::Int))
                                                ,tinyintVal "@vTinyint" (Just (11::Int))
                                                ,smallintVal "@vSmallint" (Just (12::Int))
                                                ,intVal "@vInt" (Just (13::Int))
                                                ,bigintVal "@vBigint" (Just (14::Integer))
                                                ,smallmoneyVal "@vSmallmoney" (Just (Money 1234.5678))
                                                ,moneyVal "@vMoney" (Just (Money 2234.5678))
                                                ,smalldatetimeVal "@vSmalldatetime" (Just ut1)
                                                ,datetimeVal "@vDatetime" (Just ut2)
                                                ,float24Val "@vFloat24" (Just (3234.5678::Float))
                                                ,realVal "@vReal" (Just (4234.5678::Float))
                                                ,float53Val "@vFloat53" (Just (5234.5678::Double))
                                                ,doubleVal "@vDouble" (Just (5234.5678::Double))
                                                ,uniqueidentifierVal "@vUniqueidentifier" (UUID.fromString "11112222-3333-4444-aaaa-bbbbccccffff")
                                                ,decimalVal "@vDecimal" 10 (Right (6234.5678::Fixed4))
                                                ,numericVal "@vNumeric" 11 (Right (7234.56789::Fixed5))
                                                ,charVal "@vChar" (Just ("abcd1234"::B.ByteString))
                                                ,varcharVal "@vVarchar" (Just ("abcd2234"::B.ByteString))
                                                ,textVal "@vText" (Just ("abcd3234"::B.ByteString))
                                                ,ncharVal "@vNchar" (Just ("abcd4234"::T.Text))
                                                ,nvarcharVal "@vNvarchar" (Just ("abcd5234"::T.Text))
                                                ,ntextVal "@vNtext" (Just ("abcd6234"::T.Text))
                                                ,binaryVal "@vBinary" (Just (B.pack [0,1,2,3,4,5,6,7]))
                                                ,varbinaryVal "@vVarbinary" (Just (B.pack [0,1,2,3,4,5,6,7]))
                                                ,imageVal "@vImage" (Just (B.pack [0,1,2,3,4,5,6,7]))

                                                ,bitRef "@rBit" (Nothing :: Maybe Int)
                                                ,tinyintRef "@rTinyint" (Nothing :: Maybe Int)
                                                ,smallintRef "@rSmallint" (Nothing :: Maybe Int)
                                                ,intRef "@rInt" (Nothing :: Maybe Int)
                                                ,bigintRef "@rBigint" (Nothing :: Maybe Integer)
                                                ,smallmoneyRef "@rSmallmoney" (Nothing :: Maybe Money)
                                                ,moneyRef "@rMoney" (Nothing :: Maybe Money)
                                                ,smalldatetimeRef "@rSmalldatetime" (Nothing :: Maybe UTCTime)
                                                ,datetimeRef "@rDatetime" (Nothing :: Maybe UTCTime)
                                                ,float24Ref "@rFloat24" (Nothing :: Maybe Float)
                                                ,realRef "@rReal" (Nothing :: Maybe Float)
                                                ,float53Ref "@rFloat53" (Nothing :: Maybe Double)
                                                ,doubleRef "@rDouble" (Nothing :: Maybe Double)
                                                ,uniqueidentifierRef "@rUniqueidentifier" (Nothing :: Maybe UUID)
                                                ,decimalRef "@rDecimal" 10 ((Left 4)::(Either Scale (Fixed4)))
                                                ,numericRef "@rNumeric" 11 ((Left 5)::(Either Scale (Fixed5)))
                                                ,charRef "@rChar" 20 Nothing
                                                ,varcharRef "@rVarchar" 20 Nothing
--                                                ,textRef "@rText" 20 Nothing
                                                ,ncharRef "@rNchar" 20 Nothing
                                                ,nvarcharRef "@rNvarchar" 20 Nothing
--                                                ,ntextRef "@rNtext" 20 Nothing
                                                ,binaryRef "@rBinary" 20 Nothing
                                                ,varbinaryRef "@rVarbinary" 20 Nothing
--                                                ,imageRef "@rImage" 20 Nothing
                                                )
    :: IO (RpcResponse ( (Maybe Bool)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Integer)
                       , (Maybe Money)
                       , (Maybe Money)
                       , (Maybe UTCTime)
                       , (Maybe UTCTime)
                       , (Maybe Float)
                       , (Maybe Float)
                       , (Maybe Double)
                       , (Maybe Double)
                       , (Maybe UUID)
                       , (Maybe Fixed4)
                       , (Maybe Fixed5)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       , (Maybe T.Text)
                       , (Maybe T.Text)
--                       , (Maybe T.Text)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       ) ())


  case resp of
    RpcResponse rets ((Just rBit)
                     ,(Just rTinyint)
                     ,(Just rSmallint)
                     ,(Just rInt)
                     ,(Just rBigint)
                     ,(Just rSmallmoney)
                     ,(Just rMoney)
                     ,(Just vSmalldatetime)
                     ,(Just vDatetime)
                     ,(Just vFloat24)
                     ,(Just vReal)
                     ,(Just vFloat53)
                     ,(Just vDouble)
                     ,(Just vUniqueidentifier)
                     ,(Just vDecimal)
                     ,(Just vNumeric)
                     ,(Just vChar)
                     ,(Just vVarchar)
--                     ,(Just vText)
                     ,(Just vNchar)
                     ,(Just vNvarchar)
--                     ,(Just vNtext)
                     ,(Just vBinary)
                     ,(Just vVarbinary)
--                     ,(Just vImage)
                     ) () -> do
      putStrLn $ "rets: " <> (show rets)
--      putStrLn $ "ref1: " <> (show ref1)
      putStrLn $ (show rBit)
        <> " " <> (show rTinyint)
        <> " " <> (show rSmallint)
        <> " " <> (show rInt)
        <> " " <> (show rBigint)
        <> " " <> (show rSmallmoney)
        <> " " <> (show rMoney)
        <> " " <> (show vSmalldatetime)
        <> " " <> (show vDatetime)
        <> " " <> (show vFloat24)
        <> " " <> (show vReal)
        <> " " <> (show vFloat53)
        <> " " <> (show vDouble)
        <> " " <> (show vUniqueidentifier)
        <> " " <> (show vDecimal)
        <> " " <> (show vNumeric)
        <> " " <> (show vChar)
        <> " " <> (show vVarchar)
--        <> " " <> (show vText)
        <> " " <> (show vNchar)
        <> " " <> (show vNvarchar)
--        <> " " <> (show vNtext)
        <> " " <> (show vBinary)
        <> " " <> (show vVarbinary)
--        <> " " <> (show vImage)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc4' :: Connection -> IO ()
rpc4' conn = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InputOutputDefault1"::T.Text) (bitRef "@rBit" (Nothing :: Maybe Int)
                                                ,tinyintRef "@rTinyint" (Nothing :: Maybe Int)
                                                ,smallintRef "@rSmallint" (Nothing :: Maybe Int)
                                                ,intRef "@rInt" (Nothing :: Maybe Int)
                                                ,bigintRef "@rBigint" (Nothing :: Maybe Integer)
                                                ,smallmoneyRef "@rSmallmoney" (Nothing :: Maybe Money)
                                                ,moneyRef "@rMoney" (Nothing :: Maybe Money)
                                                ,smalldatetimeRef "@rSmalldatetime" (Nothing :: Maybe UTCTime)
                                                ,datetimeRef "@rDatetime" (Nothing :: Maybe UTCTime)
                                                ,float24Ref "@rFloat24" (Nothing :: Maybe Float)
                                                ,realRef "@rReal" (Nothing :: Maybe Float)
                                                ,float53Ref "@rFloat53" (Nothing :: Maybe Double)
                                                ,doubleRef "@rDouble" (Nothing :: Maybe Double)
                                                ,uniqueidentifierRef "@rUniqueidentifier" (Nothing :: Maybe UUID)
                                                ,decimalRef "@rDecimal" 10 ((Left 4)::(Either Scale (Fixed4)))
                                                ,numericRef "@rNumeric" 11 ((Left 5)::(Either Scale (Fixed5)))
                                                ,charRef "@rChar" 20 Nothing
                                                ,varcharRef "@rVarchar" 20 Nothing
--                                                ,textRef "@rText" 20 Nothing
                                                ,ncharRef "@rNchar" 20 Nothing
                                                ,nvarcharRef "@rNvarchar" 20 Nothing
--                                                ,ntextRef "@rNtext" 20 Nothing
                                                ,binaryRef "@rBinary" 20 Nothing
                                                ,varbinaryRef "@rVarbinary" 20 Nothing
--                                                ,imageRef "@rImage" 20 Nothing
                                                )
    :: IO (RpcResponse ( (Maybe Bool)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Integer)
                       , (Maybe Money)
                       , (Maybe Money)
                       , (Maybe UTCTime)
                       , (Maybe UTCTime)
                       , (Maybe Float)
                       , (Maybe Float)
                       , (Maybe Double)
                       , (Maybe Double)
                       , (Maybe UUID)
                       , (Maybe Fixed4)
                       , (Maybe Fixed5)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       , (Maybe T.Text)
                       , (Maybe T.Text)
--                       , (Maybe T.Text)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       ) ())


  case resp of
    RpcResponse rets ((Just rBit)
                     ,(Just rTinyint)
                     ,(Just rSmallint)
                     ,(Just rInt)
                     ,(Just rBigint)
                     ,(Just rSmallmoney)
                     ,(Just rMoney)
                     ,(Just vSmalldatetime)
                     ,(Just vDatetime)
                     ,(Just vFloat24)
                     ,(Just vReal)
                     ,(Just vFloat53)
                     ,(Just vDouble)
                     ,(Just vUniqueidentifier)
                     ,(Just vDecimal)
                     ,(Just vNumeric)
                     ,(Just vChar)
                     ,(Just vVarchar)
--                     ,(Just vText)
                     ,(Just vNchar)
                     ,(Just vNvarchar)
--                     ,(Just vNtext)
                     ,(Just vBinary)
                     ,(Just vVarbinary)
--                     ,(Just vImage)
                     ) () -> do
      putStrLn $ "rets: " <> (show rets)
--      putStrLn $ "ref1: " <> (show ref1)
      putStrLn $ (show rBit)
        <> " " <> (show rTinyint)
        <> " " <> (show rSmallint)
        <> " " <> (show rInt)
        <> " " <> (show rBigint)
        <> " " <> (show rSmallmoney)
        <> " " <> (show rMoney)
        <> " " <> (show vSmalldatetime)
        <> " " <> (show vDatetime)
        <> " " <> (show vFloat24)
        <> " " <> (show vReal)
        <> " " <> (show vFloat53)
        <> " " <> (show vDouble)
        <> " " <> (show vUniqueidentifier)
        <> " " <> (show vDecimal)
        <> " " <> (show vNumeric)
        <> " " <> (show vChar)
        <> " " <> (show vVarchar)
--        <> " " <> (show vText)
        <> " " <> (show vNchar)
        <> " " <> (show vNvarchar)
--        <> " " <> (show vNtext)
        <> " " <> (show vBinary)
        <> " " <> (show vVarbinary)
--        <> " " <> (show vImage)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc5 :: Connection -> IO ()
rpc5 conn = do
  ut1 <- getCurrentTime
  let ut2 = addUTCTime (60*5) ut1
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InputOutputDefault2"::T.Text) (bitRef "@rBit" (Just (1::Int))
                                                ,tinyintRef "@rTinyint" (Just (11::Int))
                                                ,smallintRef "@rSmallint" (Just (12::Int))
                                                ,intRef "@rInt" (Just (13::Int))
                                                ,bigintRef "@rBigint" (Just (14::Integer))
                                                ,smallmoneyRef "@rSmallmoney" (Just (Money 1234.5678))
                                                ,moneyRef "@rMoney" (Just (Money 2234.5678))
                                                ,smalldatetimeRef "@rSmalldatetime" (Just ut1)
                                                ,datetimeRef "@rDatetime" (Just ut2)
                                                ,float24Ref "@rFloat24" (Just (3234.5678::Float))
                                                ,realRef "@rReal" (Just (4234.5678::Float))
                                                ,float53Ref "@rFloat53" (Just (5234.5678::Double))
                                                ,doubleRef "@rDouble" (Just (5234.5678::Double))
                                                ,uniqueidentifierRef "@rUniqueidentifier" (UUID.fromString "11112222-3333-4444-aaaa-bbbbccccffff")
                                                ,decimalRef "@rDecimal" 10 (Right (6234.5678::Fixed4))
                                                ,numericRef "@rNumeric" 11 (Right (7234.56789::Fixed5))
                                                ,charRef "@rChar" 20 (Just ("abcd1234"::B.ByteString))
                                                ,varcharRef "@rVarchar" 20 (Just ("abcd2234"::B.ByteString))
--                                                ,textRef "@rText" 20 (Just ("abcd3234"::B.ByteString))
                                                ,ncharRef "@rNchar"  20 (Just ("abcd4234"::T.Text))
                                                ,nvarcharRef "@rNvarchar" 20 (Just ("abcd5234"::T.Text))
--                                                ,ntextRef "@rNtext" 20 (Just ("abcd6234"::T.Text))
                                                ,binaryRef "@rBinary" 20 (Just (B.pack [0,1,2,3,4,5,6,7]))
                                                ,varbinaryRef "@rVarbinary"  20 (Just (B.pack [0,1,2,3,4,5,6,7]))
--                                                ,imageRef "@rImage" 20 (Just (B.pack [0,1,2,3,4,5,6,7]))
                                                )
    :: IO (RpcResponse ( (Maybe Bool)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Integer)
                       , (Maybe Money)
                       , (Maybe Money)
                       , (Maybe UTCTime)
                       , (Maybe UTCTime)
                       , (Maybe Float)
                       , (Maybe Float)
                       , (Maybe Double)
                       , (Maybe Double)
                       , (Maybe UUID)
                       , (Maybe Fixed4)
                       , (Maybe Fixed5)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       , (Maybe T.Text)
                       , (Maybe T.Text)
--                       , (Maybe T.Text)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       ) ())


  case resp of
    RpcResponse rets ((Just rBit)
                     ,(Just rTinyint)
                     ,(Just rSmallint)
                     ,(Just rInt)
                     ,(Just rBigint)
                     ,(Just rSmallmoney)
                     ,(Just rMoney)
                     ,(Just vSmalldatetime)
                     ,(Just vDatetime)
                     ,(Just vFloat24)
                     ,(Just vReal)
                     ,(Just vFloat53)
                     ,(Just vDouble)
                     ,(Just vUniqueidentifier)
                     ,(Just vDecimal)
                     ,(Just vNumeric)
                     ,(Just vChar)
                     ,(Just vVarchar)
--                     ,(Just vText)
                     ,(Just vNchar)
                     ,(Just vNvarchar)
--                     ,(Just vNtext)
                     ,(Just vBinary)
                     ,(Just vVarbinary)
--                     ,(Just vImage)
                     ) () -> do
      putStrLn $ "rets: " <> (show rets)
--      putStrLn $ "ref1: " <> (show ref1)
      putStrLn $ (show rBit)
        <> " " <> (show rTinyint)
        <> " " <> (show rSmallint)
        <> " " <> (show rInt)
        <> " " <> (show rBigint)
        <> " " <> (show rSmallmoney)
        <> " " <> (show rMoney)
        <> " " <> (show vSmalldatetime)
        <> " " <> (show vDatetime)
        <> " " <> (show vFloat24)
        <> " " <> (show vReal)
        <> " " <> (show vFloat53)
        <> " " <> (show vDouble)
        <> " " <> (show vUniqueidentifier)
        <> " " <> (show vDecimal)
        <> " " <> (show vNumeric)
        <> " " <> (show vChar)
        <> " " <> (show vVarchar)
--        <> " " <> (show vText)
        <> " " <> (show vNchar)
        <> " " <> (show vNvarchar)
--        <> " " <> (show vNtext)
        <> " " <> (show vBinary)
        <> " " <> (show vVarbinary)
--        <> " " <> (show vImage)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc5' :: Connection -> IO ()
rpc5' conn = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InputOutputDefault2"::T.Text) (bitDefRef "@rBit" :: (RpcParam (Maybe Int))
                                                ,tinyintDefRef "@rTinyint" :: (RpcParam (Maybe Int))
                                                ,smallintDefRef "@rSmallint" :: (RpcParam (Maybe Int))
                                                ,intDefRef "@rInt" :: (RpcParam (Maybe Int))
                                                ,bigintDefRef "@rBigint" :: (RpcParam (Maybe Int))
                                                ,smallmoneyDefRef "@rSmallmoney" :: (RpcParam (Maybe Money))
                                                ,moneyDefRef "@rMoney" :: (RpcParam (Maybe Money))
                                                ,smalldatetimeDefRef "@rSmalldatetime" :: (RpcParam (Maybe UTCTime))
                                                ,datetimeDefRef "@rDatetime" :: (RpcParam (Maybe UTCTime))
                                                ,float24DefRef "@rFloat24" :: (RpcParam (Maybe Float))
                                                ,realDefRef "@rReal" :: (RpcParam (Maybe Float))
                                                ,float53DefRef "@rFloat53" :: (RpcParam (Maybe Double))
                                                ,doubleDefRef "@rDouble" :: (RpcParam (Maybe Double))
                                                ,uniqueidentifierDefRef "@rUniqueidentifier" :: (RpcParam (Maybe UUID))
                                                ,decimalDefRef "@rDecimal" 10 4 :: (RpcParam(Maybe Fixed4))
                                                ,numericDefRef "@rNumeric" 11 5 :: (RpcParam(Maybe Fixed5))
                                                ,charDefRef "@rChar" 20
                                                ,varcharDefRef "@rVarchar" 20
--                                                ,textDefRef "@rText" 20
                                                ,ncharDefRef "@rNchar"  20
                                                ,nvarcharDefRef "@rNvarchar" 20
--                                                ,ntextDefRef "@rNtext" 20
                                                ,binaryDefRef "@rBinary" 20
                                                ,varbinaryDefRef "@rVarbinary"  20
--                                                ,imageDefRef "@rImage" 20
                                                )
    :: IO (RpcResponse ( (Maybe Bool)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Int)
                       , (Maybe Integer)
                       , (Maybe Money)
                       , (Maybe Money)
                       , (Maybe UTCTime)
                       , (Maybe UTCTime)
                       , (Maybe Float)
                       , (Maybe Float)
                       , (Maybe Double)
                       , (Maybe Double)
                       , (Maybe UUID)
                       , (Maybe Fixed4)
                       , (Maybe Fixed5)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       , (Maybe T.Text)
                       , (Maybe T.Text)
--                       , (Maybe T.Text)
                       , (Maybe B.ByteString)
                       , (Maybe B.ByteString)
--                       , (Maybe B.ByteString)
                       ) ())


  case resp of
    RpcResponse rets ((Just rBit)
                     ,(Just rTinyint)
                     ,(Just rSmallint)
                     ,(Just rInt)
                     ,(Just rBigint)
                     ,(Just rSmallmoney)
                     ,(Just rMoney)
                     ,(Just vSmalldatetime)
                     ,(Just vDatetime)
                     ,(Just vFloat24)
                     ,(Just vReal)
                     ,(Just vFloat53)
                     ,(Just vDouble)
                     ,(Just vUniqueidentifier)
                     ,(Just vDecimal)
                     ,(Just vNumeric)
                     ,(Just vChar)
                     ,(Just vVarchar)
--                     ,(Just vText)
                     ,(Just vNchar)
                     ,(Just vNvarchar)
--                     ,(Just vNtext)
                     ,(Just vBinary)
                     ,(Just vVarbinary)
--                     ,(Just vImage)
                     ) () -> do
      putStrLn $ "rets: " <> (show rets)
--      putStrLn $ "ref1: " <> (show ref1)
      putStrLn $ (show rBit)
        <> " " <> (show rTinyint)
        <> " " <> (show rSmallint)
        <> " " <> (show rInt)
        <> " " <> (show rBigint)
        <> " " <> (show rSmallmoney)
        <> " " <> (show rMoney)
        <> " " <> (show vSmalldatetime)
        <> " " <> (show vDatetime)
        <> " " <> (show vFloat24)
        <> " " <> (show vReal)
        <> " " <> (show vFloat53)
        <> " " <> (show vDouble)
        <> " " <> (show vUniqueidentifier)
        <> " " <> (show vDecimal)
        <> " " <> (show vNumeric)
        <> " " <> (show vChar)
        <> " " <> (show vVarchar)
--        <> " " <> (show vText)
        <> " " <> (show vNchar)
        <> " " <> (show vNvarchar)
--        <> " " <> (show vNtext)
        <> " " <> (show vBinary)
        <> " " <> (show vVarbinary)
--        <> " " <> (show vImage)
    RpcResponseError info -> putStr "error: " >> printInfo info


rpc_rv1 :: Connection -> Int -> IO ()
rpc_rv1 conn id = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_OutputTwice"::T.Text) $ intRef "@ID" (Just id)
    :: IO (RpcResponse (Only Int) ())

  case resp of
    RpcResponse rets (Only id') () -> do
      putStrLn $ "rets: " <> (show rets)
      putStrLn $ "id: " <> (show id')
    RpcResponseError info ->
      putStr "error: " >> printInfo info


rpc_rv_rs1 :: Connection -> Int -> IO ()
rpc_rv_rs1 conn id = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_OutputTwice_SelectSome"::T.Text) $ intRef "@ID" (Just id)
    :: IO (RpcResponse (Only Int) [Some])

  case resp of
    RpcResponse rets (Only id') rs -> do
      putStrLn $ "rets: " <> (show rets)
      putStrLn $ "id: " <> (show id')
      mapM_ print rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info


rpc_rs1 :: Connection -> Int -> IO ()
rpc_rs1 conn id = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_SelectSomeByID"::T.Text) $ intVal "@ID" (Just id)

  case resp of
    RpcResponse rets () rs -> do
      putStrLn $ "rets: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_rs2 :: Connection -> Int -> Int -> IO ()
rpc_rs2 conn id1 id2 = do
  (resp1, resp2) <- MSSQL.rpc conn
    ( RpcQuery ("SP_SelectSomeByID"::T.Text) $ intVal "@ID" (Just id1)
    , RpcQuery ("SP_SelectSomeByID"::T.Text) $ intVal "@ID" (Just id2)
    )

  case resp1 of
    RpcResponse rets () rs -> do
      putStrLn $ "rets1: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  putStrLn "----"

  case resp2 of
    RpcResponse rets () rs -> do
      putStrLn $ "rets2: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_rs3 :: Connection -> Int -> IO ()
rpc_rs3 conn id = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_SplitSomeByID"::T.Text) $ intVal "" (Just id)

  case resp of
    RpcResponse rets () (rs1,rs2) -> do
      putStrLn $ "rets: " <> (show rets)
      f rs1
      putStrLn "----"
      f rs2
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_sql1 :: Connection -> IO ()
rpc_sql1 conn = do
  resp <- MSSQL.rpc conn $
    RpcQuery SP_ExecuteSql $ nvarcharVal "" (Just "SELECT * FROM TSome")

  case resp of
    RpcResponse rets () rs -> do
      putStrLn $ "rets: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_sql2 :: Connection -> Int -> IO ()
rpc_sql2 conn max = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("sp_executesql"::T.Text) ( nvarcharVal "" (Just "SELECT * FROM TSome WHERE someID < @Max")
                                       , nvarcharVal "" (Just "@Max Int")
                                       , intVal "" (Just max)
                                       )

  case resp of
    RpcResponse rets () rs -> do
      putStrLn $ "rets: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_sql3 :: Connection -> Int -> Int -> IO ()
rpc_sql3 conn min max = do
  resp <- MSSQL.rpc conn $
    RpcQuery (0xa::Word16) ( nvarcharVal "" (Just "SELECT * FROM TSome WHERE @Min < someID AND someID < @Max")
                           , nvarcharVal "" (Just "@Min Int, @Max Int")
                           , intVal "@Min" (Just min)
                           , intVal "@Max" (Just max)
                           )

  case resp of
    RpcResponse rets () rs -> do
      putStrLn $ "rets: " <> (show rets)
      f rs
    RpcResponseError info ->
      putStr "error: " >> printInfo info

  where
    f :: [Some] -> IO ()
    f rs = forM_ rs print



rpc_sql4 :: Connection -> IO ()
rpc_sql4 conn = do
  resp <- MSSQL.rpc conn $
    RpcQuery (0xa::Word16) ( nvarcharVal "" (Just "UPDATE TTypes2 SET t2GUID = @G1, t2GUIDN = @G2 WHERE t2ID = 1")
                           , nvarcharVal "" (Just "@G1 UniqueIdentifier, @G2 UniqueIdentifier")
                           , uniqueidentifierVal "@G1" (UUID.fromString "3EC68940-4275-4F2C-96F6-975ACD2DB037")
                           , uniqueidentifierVal "@G2" (UUID.fromString "3EC68940-4275-4F2C-96F6-975ACD2DB037")
                           )

  case resp of
    RpcResponse rets () () -> do
      putStrLn $ "rets: " <> (show rets)
    RpcResponseError info ->
      putStr "error: " >> printInfo info


rpc_insert1 :: Connection -> T.Text -> T.Text -> IO ()
rpc_insert1 conn title content = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InsertSome"::T.Text) ( nvarcharVal "@Title" (Just title)
                                       , nvarcharVal "@Content" (Just content)
--                                       , moneyVal "@Price" (Just 1111.0) -- overwrite default
                                       , intRef "@ID" (Nothing :: Maybe Int)
                                       )
    :: IO (RpcResponse (Only Int) ())

  case resp of
    RpcResponse rets (Only id) () -> do
      putStrLn $ "rets: " <> (show rets)
      putStrLn $ "id: " <> (show id)
    RpcResponseError info ->
      putStr "error: " >> printInfo info



rpc_insert2 :: Connection -> T.Text -> T.Text -> IO ()
rpc_insert2 conn title content = do
  resp <- MSSQL.rpc conn $
    RpcQuery ("SP_InsertSomeDate"::T.Text) ( nvarcharVal "@Title" (Just title)
                                           , nvarcharVal "@Content" (Just content)
                                           , intRef "@ID" (Nothing :: Maybe Int)
                                           , datetimeRef "@Created" (Nothing :: Maybe UTCTime)
                                           )
    :: IO (RpcResponse (Int,UTCTime) ())

  case resp of
    RpcResponse rets (id,time) () -> do
      putStrLn $ "rets: " <> (show rets)
      putStrLn $ "id: " <> (show id)
      putStrLn $ "time: " <> (show time)
    RpcResponseError info ->
      putStr "error: " >> printInfo info






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
    f :: [(Int,UUID,Fixed0,Fixed1,Fixed37,Fixed38,Fixed0,Fixed1,Fixed37,Fixed38,(Maybe UUID),(Maybe Fixed0),(Maybe Fixed1),(Maybe Fixed37),(Maybe Fixed38),(Maybe Fixed0),(Maybe Fixed1),(Maybe Fixed37),(Maybe Fixed38))] -> IO ()
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


test_transaction1 :: Connection -> IO ()
test_transaction1 conn = withTransaction conn $ do
  _ <- MSSQL.sql conn $ "UPDATE TSome SET somePrice = somePrice -100 WHERE someID = 1" :: IO ()
  _ <- MSSQL.sql conn $ "UPDATE TSome SET somePrice = somePrice +100 WHERE someID = 2" :: IO ()
  return ()


test_transaction2 :: Connection -> IO ()
test_transaction2 conn = withTransaction conn $ do
  _ <- MSSQL.sql conn $ "UPDATE TSome SET somePrice = somePrice -100 WHERE someID = 1" :: IO ()
  _ <- MSSQL.sql conn $ "UPDATE TSome SET somePrice = (somePrice +100) / 0 WHERE someID = 2" :: IO ()
  return ()

