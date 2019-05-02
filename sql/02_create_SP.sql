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


