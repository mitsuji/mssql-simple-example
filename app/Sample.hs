{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Sample where

import Network.Socket (withSocketsDo)
import Control.Exception (bracket)

import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query

import qualified Data.Text.Lazy as LT
import Database.Tds.Message
import Data.Time (UTCTime(..))
import Data.Monoid (mconcat)
import Text.RawString.QQ (r)

import qualified Data.Text as T
import Data.Word (Word16)

sample :: IO ()
sample = do
  let info = defaultConnectInfo { connectHost = "192.168.0.1"
                                , connectPort = "1433"
                                , connectDatabase = "some_database"
                                , connectUser = "some_user"
                                , connectPassword = "some_password"
                                }
  withSocketsDo $
    bracket (connect info) close $ \conn -> do
--    rs <- sql conn "SELECT 2 + 2" :: IO [Only Int]
--    print rs
    print =<< sql_select1 conn
--    print =<< sql_select2 conn
--    print =<< sql_select3 conn
--    print =<< sql_select4 conn
--    print =<< sql_select5 conn
--    print =<< sql_count1 conn
--    sql_discard1 conn
--    sql_create_table1 conn
--    sql_insert1 conn
--    print =<< sql_status1 conn
--    print =<< sql_select6 conn
--    print =<< rpc_sql_select1 conn
--    print =<< rpc_sql_select2 conn 5
--    print =<< rpc_sql_select3 conn 5 13


sql_select1 :: Connection -> IO Int
sql_select1 conn = do
    [Only i] <- sql conn "SELECT 2 + 2" :: IO [Only Int]
    return i


sql_select2 :: Connection -> IO (Maybe Int)
sql_select2 conn = do
    [Only mi] <- sql conn "SELECT 6 / 2" :: IO [Only (Maybe Int)]
    return mi


sql_select3 :: Connection -> IO (Maybe Int)
sql_select3 conn = do
    [Only mi] <- sql conn "SELECT 6 / 0" :: IO [Only (Maybe Int)]
    return mi


sql_select4 :: Connection -> IO [(Int,String,LT.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)]
sql_select4 conn = sql conn "SELECT * FROM TSome ORDER BY somePrice"


data Some = Some { someID :: Int
                 , someTitle :: LT.Text
                 , someContent :: LT.Text
                 , somePrice :: Money
                 , someCreated:: UTCTime
                 , someModified:: Maybe UTCTime
                 , someDeleted:: Maybe UTCTime
                 }
          deriving (Show)

instance Row Some where
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

sql_select5 :: Connection -> IO [Some]
sql_select5 conn = sql conn "SELECT TOP 10 * FROM TSome ORDER BY somePrice DESC"


sql_count1 :: Connection -> IO Int
sql_count1 conn = do
  RowCount rc <- sql conn "UPDATE TSome SET somePrice = somePrice + 100 WHERE someID < 5"
  return rc


sql_discard1 :: Connection -> IO ()
sql_discard1 conn = sql conn "UPDATE TSome SET somePrice = somePrice + 100 WHERE someID < 5"




sql_create_table1 :: Connection -> IO ()
sql_create_table1 conn = do
  sql conn "DROP TABLE TSome" :: IO ()
  sql conn [r|
CREATE TABLE TSome (
	someID int IDENTITY(1,1) NOT NULL,
	someTitle nvarchar(40) NOT NULL,
	someContent ntext NOT NULL,
	somePrice money NOT NULL,
	someCreated datetime NOT NULL,
	someModified datetime NULL,
	someDeleted datetime NULL,

	CONSTRAINT PK_TSome PRIMARY KEY CLUSTERED (
		someID ASC
	)
	WITH (
		PAD_INDEX  = OFF,
		STATISTICS_NORECOMPUTE  = OFF,
		IGNORE_DUP_KEY = OFF,
		ALLOW_ROW_LOCKS  = ON,
		ALLOW_PAGE_LOCKS  = ON
	) ON [PRIMARY]

) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
|]


sql_insert1 :: Connection -> IO ()
sql_insert1 conn = sql conn [r|
BEGIN TRAN;
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
INSERT INTO TSome(someTitle,someContent,somePrice,someCreated) VALUES('title','content',12345.60,GETDATE());
COMMIT TRAN;
|]


sql_status1 :: Connection -> IO Int
sql_status1 conn = do
  ReturnStatus rets <- sql conn "EXEC SP_Input1 @Val1=3"
  return rets


sql_select6 :: Connection -> IO ([Some],[Some])
sql_select6 conn =
  sql conn $ mconcat ["SELECT * FROM TSome WHERE someID < 8 ORDER BY someID;",
                      "SELECT * FROM TSome WHERE 8 <= someID AND someID < 12 ORDER BY someID DESC;"]





rpc_sql_select1 :: Connection -> IO (RpcResponse () [Some])
rpc_sql_select1 conn = rpc conn $
                       RpcQuery ("sp_executesql"::T.Text) $ nvarcharVal "" (Just "SELECT * FROM TSome")


rpc_sql_select2 :: Connection -> Int -> IO (RpcResponse () [Some])
rpc_sql_select2 conn max = rpc conn $ RpcQuery SP_ExecuteSql
                           ( nvarcharVal "" (Just "SELECT * FROM TSome WHERE someID < @Max")
                           , nvarcharVal "" (Just "@Max Int")
                           , intVal "" (Just max)
                           )


rpc_sql_select3 :: Connection -> Int -> Int -> IO (RpcResponse () [Some])
rpc_sql_select3 conn min max = rpc conn $ RpcQuery (0xa::Word16)
                               ( nvarcharVal "" (Just "SELECT * FROM TSome WHERE @Min < someID AND someID < @Max")
                               , nvarcharVal "" (Just "@Min Int, @Max Int")
                               , intVal "@Min" (Just min)
                               , intVal "@Max" (Just max)
                               )
