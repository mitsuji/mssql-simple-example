USE [mss-test]
GO

CREATE PROCEDURE SP_SelectSomeByID 
	@ID int
AS
BEGIN
	SELECT * FROM TSome WHERE someID = @ID
END

GO


CREATE PROCEDURE SP_SplitSomeByID
	@ID int
AS
BEGIN
	SELECT * FROM TSome WHERE someID < @ID;
	SELECT * FROM TSome WHERE someID >= @ID;
END

GO


CREATE PROCEDURE SP_InsertSome
	@Title nvarchar(40),
	@Content ntext,
	@Price money = 1000.0,
	@ID int OUT
AS
BEGIN
	INSERT INTO TSome (someTitle, someContent, somePrice, someCreated)
	            VALUES (@Title, @Content, @Price, GETDATE())
	            
	SET @ID = SCOPE_IDENTITY()
END

GO


CREATE PROCEDURE SP_InsertSomeDate
	@Title nvarchar(40),
	@Content ntext,
	@Price money = 1000.0,
	@ID int OUT,
	@Created datetime OUT
AS
BEGIN
	SET @Created = GETDATE()
	INSERT INTO TSome (someTitle, someContent, somePrice, someCreated)
	            VALUES (@Title, @Content, @Price, @Created)
	            
	SET @ID = SCOPE_IDENTITY()
END

GO


CREATE PROCEDURE SP_Input1
	@Val1 int
AS
BEGIN
	RETURN @Val1 + 100
END

GO


CREATE PROCEDURE SP_Output1 
	@Ref1 int OUTPUT
AS
BEGIN
	SET @Ref1 = @Ref1 + 33
	Return 0
END

GO


CREATE PROCEDURE SP_OutputTwice
	@ID int OUTPUT
AS
BEGIN

	EXEC SP_Output1 @Ref1 = @ID OUTPUT;
	EXEC SP_Output1 @Ref1 = @ID OUTPUT;
	
	RETURN 333;
END

GO


CREATE PROCEDURE SP_OutputTwice_SelectSome
	@ID int OUTPUT
AS
BEGIN

	EXEC SP_Output1 @Ref1 = @ID OUTPUT;
	EXEC SP_Output1 @Ref1 = @ID OUTPUT;
	
	SELECT * FROM TSome;
	
	RETURN 333;
END

GO


CREATE PROCEDURE SP_InputDefault1
	@Val1 int = 11
AS
BEGIN
	RETURN @Val1 + 100
END

GO


CREATE PROCEDURE SP_OutputDefault1
	@Ref1 int = 12 OUTPUT
AS
BEGIN
	SET @Ref1 = @Ref1 + 33
	Return 0
END

GO


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

GO


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

GO


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

GO


