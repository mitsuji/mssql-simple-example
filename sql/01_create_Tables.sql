USE [mss-test]
GO

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

GO


CREATE TABLE TTypes1 (
	t1ID int NOT NULL,
	
	t1Bit bit NOT NULL,
	t1Int1 tinyint NOT NULL,
	t1Int2 smallint NOT NULL,
	t1Int4 int NOT NULL,
	t1Int8 bigint NOT NULL,
	t1Money4 smallmoney NOT NULL,
	t1Money8 money NOT NULL,
	t1DateTime4 smalldatetime NOT NULL,
	t1DateTime8 datetime NULL,
	t1Flt4 real NOT NULL,
	t1Flt8 float NOT NULL,
	
	t1BitN bit NULL,
	t1IntN1 tinyint NULL,
	t1IntN2 smallint NULL,
	t1IntN4 int NULL,
	t1IntN8 bigint NULL,
	t1MoneyN4 smallmoney NULL,
	t1MoneyN8 money NULL,
	t1DateTimeN4 smalldatetime NULL,
	t1DateTimeN8 datetime NULL,
	t1FltN4 real NULL,
	t1FltN8 float NULL,
	
	CONSTRAINT PK_TTypes1 PRIMARY KEY CLUSTERED (
		t1ID ASC
	)
	WITH (
		PAD_INDEX  = OFF,
		STATISTICS_NORECOMPUTE  = OFF,
		IGNORE_DUP_KEY = OFF,
		ALLOW_ROW_LOCKS  = ON,
		ALLOW_PAGE_LOCKS  = ON
	) ON [PRIMARY]

) ON [PRIMARY]

GO


CREATE TABLE TTypes2 (
	t2ID int NOT NULL,
	
	t2GUID uniqueidentifier NOT NULL,
	t2Decimal0 decimal(38, 0) NOT NULL,
	t2Decimal1 decimal(38, 1) NOT NULL,
	t2Decimal37 decimal(38, 37) NOT NULL,
	t2Decimal38 decimal(38, 38) NOT NULL,
	t2Numeric0 numeric(38, 0) NOT NULL,
	t2Numeric1 numeric(38, 1) NOT NULL,
	t2Numeric37 numeric(38, 37) NOT NULL,
	t2Numeric38 numeric(38, 38) NOT NULL,
	
	t2GUIDN uniqueidentifier NULL,
	t2DecimalN0 decimal(38, 0) NULL,
	t2DecimalN1 decimal(38, 1) NULL,
	t2DecimalN37 decimal(38, 37) NULL,
	t2DecimalN38 decimal(38, 38) NULL,
	t2NumericN0 numeric(38, 0) NULL,
	t2NumericN1 numeric(38, 1) NULL,
	t2NumericN37 numeric(38, 37) NULL,
	t2NumericN38 numeric(38, 38) NULL,
	
	CONSTRAINT PK_TTypes2 PRIMARY KEY CLUSTERED (
		t2ID ASC
	)
	WITH (
		PAD_INDEX  = OFF,
		STATISTICS_NORECOMPUTE  = OFF,
		IGNORE_DUP_KEY = OFF,
		ALLOW_ROW_LOCKS  = ON,
		ALLOW_PAGE_LOCKS  = ON
	) ON [PRIMARY]

) ON [PRIMARY]

GO


CREATE TABLE TTypes3 (
	t3ID int NOT NULL,
	
	t3BigChar char(10) NOT NULL,
	t3BigVarChar varchar(10) NOT NULL,
	t3Text text NOT NULL,
	t3NChar nchar(10) NOT NULL,
	t3NVarChar nvarchar(10) NOT NULL,
	t3NText ntext NOT NULL,
	t3BigBinary binary(10) NOT NULL,
	t3BigVarBinary varbinary(10) NOT NULL,
	t3Image image NOT NULL,
	
	t3BigCharN char(10) NULL,
	t3BigVarCharN varchar(10) NULL,
	t3TextN text NULL,
	t3NCharN nchar(10) NULL,
	t3NVarCharN nvarchar(10) NULL,
	t3NTextN ntext NULL,
	t3BigBinaryN binary(10) NULL,
	t3BigVarBinaryN varbinary(10) NULL,
	t3ImageN image NULL,

	CONSTRAINT PK_TTypes3 PRIMARY KEY CLUSTERED (
		t3ID ASC
	)
	WITH (
		PAD_INDEX  = OFF,
		STATISTICS_NORECOMPUTE  = OFF,
		IGNORE_DUP_KEY = OFF,
		ALLOW_ROW_LOCKS  = ON,
		ALLOW_PAGE_LOCKS  = ON
	) ON [PRIMARY]
	
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO


