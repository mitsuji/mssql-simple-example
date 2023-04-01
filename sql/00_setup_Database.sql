USE [master]
GO

CREATE DATABASE [mss-test]
GO

CREATE LOGIN [mss-test_admin] WITH PASSWORD=N'msst_adm-1234'
GO
CREATE LOGIN [mss-test_rw] WITH PASSWORD=N'msst_rw-1234'
GO

USE [mss-test]
GO

CREATE USER [mss-test_admin] FOR LOGIN [mss-test_admin] WITH DEFAULT_SCHEMA=[dbo]
GO
CREATE USER [mss-test_rw] FOR LOGIN [mss-test_rw] WITH DEFAULT_SCHEMA=[dbo]
GO

ALTER ROLE [db_owner] ADD MEMBER [mss-test_admin]
ALTER ROLE [db_datareader] ADD MEMBER [mss-test_rw]
ALTER ROLE [db_datawriter] ADD MEMBER [mss-test_rw]
GO
