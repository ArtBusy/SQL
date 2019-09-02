USE [NAC_dev]
GO

/****** Object:  StoredProcedure [dbo].[GenerateTriggers_POA]    Script Date: 9/2/2019 12:38:10 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



   
CREATE PROC [dbo].[GenerateTriggers_POA]   
 @Schemaname Sysname = 'dbo'   
,@Tablename  Sysname   
,@GenerateScriptOnly    bit = 1  
,@ForceDropAuditTable   bit = 0  
,@IgnoreExistingColumnMismatch   bit = 0  
,@DontAuditforUsers NVARCHAR(4000) =  ''
,@DontAuditforColumns NVARCHAR(4000) =  ''
,@EserviceId NVARCHAR(225) = 'OpenCallId'
--EXEC GenerateTriggers @Schemaname = 'dbo',@Tablename  = 'OpenCall_ProposalaProgram', @GenerateScriptOnly = 0,@EserviceId = 'OpenCallId'
AS   
   
SET NOCOUNT ON   
   
/*   
Parameters   
@Schemaname            - SchemaName to which the table belongs to. Default value 'dbo'.   
@Tablename            - TableName for which the procs needs to be generated.   
@GenerateScriptOnly - When passed 1 , this will generate the scripts alone..   
                      When passed 0 , this will create the audit tables and triggers in the current database.   
                      Default value is 1   
@ForceDropAuditTable - When passed 1 , will drop the audit table and recreate 
                       When passed 0 , will generate the alter scripts 
                       Default value is 0 
@IgnoreExistingColumnMismatch - When passed 1 , will not stop with the error on the mismatch of existing column and will create the trigger. 
                                When passed 0 , will stop with the error on the mismatch of existing column. 
                                Default value is 0 
@DontAuditforUsers - Pass the UserName as comma seperated for whom the audit is not required.
					 Default value is '' which will do audit for all the users.

@DontAuditforColumns - Pass the ColumnNames as comma seperated for which the audit is not required.
      Default value is '' which will do audit for all the users.
*/   
   
DECLARE @SQL VARCHAR(MAX)   
DECLARE @SQLTrigger VARCHAR(MAX)   
DECLARE @ErrMsg VARCHAR(MAX)
DECLARE @AuditTableName SYSNAME   
DECLARE @QuotedSchemaName SYSNAME
DECLARE @QuotedTableName SYSNAME
DECLARE @QuotedAuditTableName SYSNAME
DECLARE @InsertTriggerName SYSNAME
DECLARE @UpdateTriggerName SYSNAME
DECLARE @DeleteTriggerName SYSNAME
DECLARE @QuotedInsertTriggerName SYSNAME
DECLARE @QuotedUpdateTriggerName SYSNAME
DECLARE @QuotedDeleteTriggerName SYSNAME
DECLARE @DontAuditforUsersTmp NVARCHAR(4000)

SELECT @AuditTableName =  @Tablename + '_Audit'   
SELECT @QuotedSchemaName = QUOTENAME(@Schemaname)
SELECT @QuotedTableName = QUOTENAME(@Tablename)
SELECT @QuotedAuditTableName = QUOTENAME(@AuditTableName)
SELECT @InsertTriggerName = @Tablename + '_Insert'  
SELECT @UpdateTriggerName = @Tablename + '_Update'  
SELECT @DeleteTriggerName = @Tablename + '_Delete'  
SELECT @QuotedInsertTriggerName = QUOTENAME(@InsertTriggerName)
SELECT @QuotedUpdateTriggerName = QUOTENAME(@UpdateTriggerName)
SELECT @QuotedDeleteTriggerName = QUOTENAME(@DeleteTriggerName)

IF LTRIM(RTRIM(@DontAuditforUsers)) <> ''
BEGIN

	IF RIGHT(@DontAuditforUsers,1) = ','
	BEGIN
		SELECT @DontAuditforUsersTmp = LEFT(@DontAuditforUsers,LEN(@DontAuditforUsers) -1)
	END
	ELSE
	BEGIN
		SELECT @DontAuditforUsersTmp = @DontAuditforUsers
	END
	
	SELECT @DontAuditforUsersTmp = REPLACE(@DontAuditforUsersTmp,',',''',''')

END


SELECT @DontAuditforColumns =',' + UPPER(@DontAuditforColumns) + ','

IF NOT EXISTS (SELECT 1    
             FROM sys.objects    
            WHERE Name= @TableName   
              AND Schema_id=Schema_id(@Schemaname)   
              AND Type = 'U') 
BEGIN
	SELECT @ErrMsg = @QuotedSchemaName + '.' + @QuotedTableName + ' Table Not Found ' 
	RAISERROR(@ErrMsg ,16,1)
	RETURN
END

   
----------------------------------------------------------------------------------------------------------------------   
-- Audit Create OR Alter table    
----------------------------------------------------------------------------------------------------------------------   
  
DECLARE @ColList VARCHAR(MAX)  
DECLARE @InsertColList VARCHAR(MAX)  
DECLARE @UpdateCheck VARCHAR(MAX)  
 
DECLARE @NewAddedCols TABLE 
( 
 ColumnName SYSNAME 
,DataType   SYSNAME 
,CharLength INT 
,Collation  SYSNAME NULL 
,ChangeType VARCHAR(20) NULL 
,MainTableColumnName SYSNAME  NULL 
,MainTableDataType   SYSNAME  NULL 
,MainTableCharLength INT  NULL 
,MainTableCollation SYSNAME  NULL 
,AuditTableColumnName SYSNAME  NULL 
,AuditTableDataType   SYSNAME  NULL 
,AuditTableCharLength INT  NULL 
,AuditTableCollation SYSNAME  NULL 
) 
 
  
SELECT @ColList = ''  
SELECT @UpdateCheck = ' '  
SELECT @SQL = '' 
SELECT @InsertColList = ''

 
SELECT @ColList = @ColList +   CASE SC.is_identity  
                                           WHEN 1 THEN 'CONVERT(' + ST.name + ',' + QUOTENAME(SC.name) + ') as ' + QUOTENAME(SC.name)  
                                           ELSE QUOTENAME(SC.name)  
                                           END + ','  
				, @InsertColList = @InsertColList + QUOTENAME(SC.name) + ','
                ,  @UpdateCheck = @UpdateCheck + 
								  CASE 
								  WHEN CHARINDEX(',' + UPPER(SC.NAME) + ',',@DontAuditforColumns) = 0 THEN 
								  'CASE WHEN exists(Select ' + QUOTENAME(SC.name) + ' from Inserted Except select ' + QUOTENAME(SC.name) + ' from Deleted) THEN ''' + QUOTENAME(SC.name) + '-'' ELSE '''' END + ' + CHAR(10)
								  --'CASE WHEN UPDATE(' + QUOTENAME(SC.name) + ') THEN ''' + QUOTENAME(SC.name) + '-'' ELSE '''' END + ' +  CHAR(10)
								  --WHEN CHARINDEX(',' + UPPER(SC.NAME) + ',',@DontAuditforColumns) = 0 THEN 'CASE WHEN (SELECT ' + QUOTENAME(SC.name) + ' FROM inserted) !=  (SELECT ' + QUOTENAME(SC.name) + ' FROM deleted) THEN ''' + QUOTENAME(SC.name) + '-'' ELSE '''' END + ' +  CHAR(10)    
								  ELSE ''
								  END
              FROM SYS.COLUMNS SC  
              JOIN SYS.OBJECTS SO  
                ON SC.object_id = SO.object_id     
              JOIN SYS.schemas SCH  
                ON SCH.schema_id = SO.schema_id  
              JOIN SYS.types ST  
                ON ST.user_type_id = SC.user_type_id  
               AND ST.system_type_id = SC.system_type_id   
             WHERE SCH.Name = @Schemaname   
               AND SO.name  = @Tablename   
               AND UPPER(ST.name)  <> UPPER('timestamp')  
  
            SELECT @ColList = SUBSTRING(@ColList,1,LEN(@ColList)-1)  
			SELECT @UpdateCheck = SUBSTRING(@UpdateCheck,1,LEN(@UpdateCheck)-3)  
			SELECT @InsertColList = SUBSTRING(@InsertColList,1,LEN(@InsertColList)-1)  
 
            SELECT @InsertColList = @InsertColList + ',AuditDataState,AuditDMLAction,AuditUser,AuditUserId,AuditDateTime,UpdateColumns' 
 
IF EXISTS (SELECT 1    
             FROM sys.objects    
            WHERE Name= @AuditTableName   
              AND Schema_id=Schema_id(@Schemaname)   
              AND Type = 'U')  AND @ForceDropAuditTable = 0 
 BEGIN 
 
            ----------------------------------------------------------------------------------------------------------------------   
            -- Get the comparision metadata for Main and Audit Tables 
            ----------------------------------------------------------------------------------------------------------------------   
 
            INSERT INTO @NewAddedCols 
            (ColumnName,DataType,CharLength,Collation,ChangeType,MainTableColumnName 
            ,MainTableDataType,MainTableCharLength,MainTableCollation,AuditTableColumnName,AuditTableDataType,AuditTableCharLength,AuditTableCollation) 
            SELECT ISNULL(MainTable.ColumnName,AuditTable.ColumnName) 
                  ,ISNULL(MainTable.DataType,AuditTable.DataType) 
                  ,ISNULL(MainTable.CharLength,AuditTable.CharLength) 
                  ,ISNULL(MainTable.Collation,AuditTable.Collation) 
                  ,CASE  
                   WHEN MainTable.ColumnName IS NULL THEN 'Deleted' 
                   WHEN AuditTable.ColumnName IS NULL THEN 'Added' 
                   ELSE NULL 
                   END  
                  ,MainTable.ColumnName 
                  ,MainTable.DataType 
                  ,MainTable.CharLength 
                  ,MainTable.Collation 
                  ,AuditTable.ColumnName 
                  ,AuditTable.DataType 
                  ,AuditTable.CharLength 
                  ,AuditTable.Collation 
              FROM 
     
            ( 
            SELECT SC.Name As ColumnName,ST.Name as DataType,SC.is_identity as isIdentity,SC.Max_length as CharLength,SC.Collation_Name as Collation 
              FROM SYS.COLUMNS SC  
              JOIN SYS.OBJECTS SO  
                ON SC.object_id = SO.object_id     
              JOIN SYS.schemas SCH  
                ON SCH.schema_id = SO.schema_id  
              JOIN SYS.types ST  
                ON ST.user_type_id = SC.user_type_id  
               AND ST.system_type_id = SC.system_type_id   
             WHERE SCH.Name = @Schemaname   
               AND SO.name  = @Tablename   
               AND UPPER(ST.name)  <> UPPER('timestamp')  
            ) MainTable 
            FULL OUTER JOIN 
            ( 
            SELECT SC.Name As ColumnName,ST.Name as DataType,SC.is_identity as isIdentity,SC.Max_length as CharLength,SC.Collation_Name as Collation 
              FROM SYS.COLUMNS SC  
              JOIN SYS.OBJECTS SO  
                ON SC.object_id = SO.object_id     
              JOIN SYS.schemas SCH  
                ON SCH.schema_id = SO.schema_id  
              JOIN SYS.types ST  
                ON ST.user_type_id = SC.user_type_id  
               AND ST.system_type_id = SC.system_type_id   
             WHERE SCH.Name = @Schemaname   
               AND SO.name  = @AuditTableName   
               AND UPPER(ST.name)  <> UPPER('timestamp')  
               AND SC.Name NOT IN ('AuditDataState','AuditDMLAction','AuditUser','AuditUserId','AuditDateTime','UpdateColumns') 
            ) AuditTable 
            ON MainTable.ColumnName = AuditTable.ColumnName 
          
         ----------------------------------------------------------------------------------------------------------------------   
        -- Find data type changes between table 
        ----------------------------------------------------------------------------------------------------------------------   
 
            IF EXISTS ( SELECT * FROM @NewAddedCols NC 
                         WHERE NC.MainTableColumnName = NC.AuditTableColumnName 
                           AND ( 
                               NC.MainTableDataType   <> NC.AuditTableDataType 
                               OR 
                               NC.MainTableCharLength  > NC.AuditTableCharLength 
                               OR 
                               NC.MainTableCollation  <> NC.AuditTableCollation 
                               ) 
                ) 
            BEGIN 
                SELECT CONVERT(VARCHAR(50), 
                           CASE 
                           WHEN NC.MainTableDataType   <> NC.AuditTableDataType   THEN 'DataType Mismatch' 
                           WHEN NC.MainTableCharLength  > NC.AuditTableCharLength THEN 'Length in maintable is greater than Audit Table' 
                           WHEN NC.MainTableCollation  <> NC.AuditTableCollation  THEN 'Collation Difference' 
                           END) AS Mismatch 
                      ,NC.MainTableColumnName 
                      ,NC.MainTableDataType 
                      ,NC.MainTableCharLength 
                      ,NC.MainTableCollation 
                      ,NC.AuditTableColumnName 
                      ,NC.AuditTableDataType 
                      ,NC.AuditTableCharLength 
                      ,NC.AuditTableCollation 
                  FROM @NewAddedCols NC 
                 WHERE NC.MainTableColumnName = NC.AuditTableColumnName 
                   AND ( 
                       NC.MainTableDataType   <> NC.AuditTableDataType 
                       OR 
                       NC.MainTableCharLength  > NC.AuditTableCharLength 
                       OR 
                       NC.MainTableCollation  <> NC.AuditTableCollation 
                       ) 
 
                RAISERROR('There are differences in Datatype or Lesser Length or Collation difference between the Main table and Audit Table. Please refer the output',16,1) 
                IF @IgnoreExistingColumnMismatch = 0 
                BEGIN 
                    RETURN 
                END 
            END 
 
        ----------------------------------------------------------------------------------------------------------------------   
        -- Find the new and deleted columns  
        ----------------------------------------------------------------------------------------------------------------------   
 
          IF EXISTS(SELECT * FROM @NewAddedCols WHERE ChangeType IS NOT NULL) 
          BEGIN 
 
                SELECT @SQL = @SQL +   'ALTER TABLE ' + @QuotedSchemaName + '.' + @QuotedAuditTableName  
                                   + CASE 
                                     WHEN NC.ChangeType ='Added' THEN ' ADD '  + QUOTENAME(NC.ColumnName) + ' ' + NC.DataType + ' '  
                                     +  CASE  
                                        WHEN NC.DataType IN ('char','varchar','nchar','nvarchar') AND NC.CharLength = -1 THEN '(max) COLLATE ' + NC.Collation + ' NULL ' 
                                        WHEN NC.DataType IN ('char','varchar') THEN '(' + CONVERT(VARCHAR(5),NC.CharLength) + ') COLLATE ' + NC.Collation + ' NULL ' 
                                        WHEN NC.DataType IN ('nchar','nvarchar') THEN '(' + CONVERT(VARCHAR(5),NC.CharLength/2) + ') COLLATE ' + NC.Collation + ' NULL ' 
                                        ELSE '' 
                                        END 
                                     WHEN NC.ChangeType ='Deleted' THEN ' DROP COLUMN '  + QUOTENAME(NC.ColumnName)  
                                     END + CHAR(10) 
                                      
                  FROM @NewAddedCols NC 
                  WHERE NC.ChangeType IS NOT NULL 
          END 
 
 
 END 
 ELSE 
 BEGIN 
  
            SELECT @SQL = '  IF EXISTS (SELECT 1    
										  FROM sys.objects    
									     WHERE Name=''' + @AuditTableName + '''   
										   AND Schema_id=Schema_id(''' + @Schemaname + ''')   
										   AND Type = ''U'')   
							DROP TABLE ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + '
  
                    SELECT ' + @ColList + '   
                        ,AuditDataState=CONVERT(VARCHAR(10),'''')    
                        ,AuditDMLAction=CONVERT(VARCHAR(10),'''')     
                        ,AuditUser =CONVERT(SYSNAME,'''')  
						,AuditUserId =CONVERT(VARCHAR(255),'''')
                        ,AuditDateTime=CONVERT(DATETIME,''01-JAN-1900'')   
                        ,UpdateColumns = CONVERT(VARCHAR(MAX),'''')  
                        Into ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + '   
                    FROM ' + @QuotedSchemaName + '.' + @QuotedTableName +'   
                    WHERE 1=2 '   
END 


 
IF @GenerateScriptOnly = 1   
BEGIN   
    PRINT REPLICATE ('-',200)   
    PRINT '--Create \ Alter Script Audit table for ' + @QuotedSchemaName + '.' + @QuotedTableName   
    PRINT REPLICATE ('-',200)   
    PRINT @SQL   
    IF LTRIM(RTRIM(@SQL)) <> '' 
    BEGIN 
        PRINT 'GO'  
    END 
    ELSE 
    BEGIN 
        PRINT '-- No changes in table structure' 
    END  
END   
ELSE   
BEGIN   
    IF RTRIM(LTRIM(@SQL)) = '' 
    BEGIN 
        PRINT 'No Table Changes Found'  
    END 
    ELSE 
    BEGIN 
        PRINT 'Creating \ Altered Audit table for ' + @QuotedSchemaName + '.' + @QuotedTableName   
        EXEC(@SQL)   
        PRINT 'Audit table ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + ' Created \ Altered succesfully'   
    END 
END   
   
   
----------------------------------------------------------------------------------------------------------------------   
-- Create Insert Trigger   
----------------------------------------------------------------------------------------------------------------------   
   
   
SELECT @SQL = '   
IF EXISTS (SELECT 1    
             FROM sys.objects    
            WHERE Name=''' + @Tablename + '_Insert' + '''   
              AND Schema_id=Schema_id(''' + @Schemaname + ''')   
              AND Type = ''TR'')   
DROP TRIGGER ' + @QuotedSchemaName + '.' + @QuotedInsertTriggerName    
SELECT @SQLTrigger = '   
CREATE TRIGGER '  + @QuotedSchemaName + '.' + @QuotedInsertTriggerName + '
ON '  + @QuotedSchemaName + '.' + @QuotedTableName + '   
FOR INSERT   
AS   
' 


IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' IF SUSER_NAME() NOT IN (''' + @DontAuditforUsersTmp + ''')'
SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' BEGIN'

END

IF EXISTS( SELECT 1 FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '' + 
	REPLACE(REPLACE(@QuotedTableName,'[',''),']','') + '' AND  COLUMN_NAME = 'CreatedBy')
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger  + CHAR(10) + '
	DECLARE @RequestUserType int, @RequestInsertUser NVARCHAR(225),@RequestInsertUserId NVARCHAR(225) 
		
	--(case when exists (select *
 --                      from INFORMATION_SCHEMA.COLUMNS 
 --                      where TABLE_NAME = '''+@Tablename+''' and
 --                            COLUMN_NAME = ''UserType'' ) then UserType  else 1 end)
	Select @RequestUserType = UserType,@RequestInsertUserId=CreatedBy FROM INSERTED
	if(@RequestUserType=0)
		SET @RequestInsertUser = (SELECT TOP 1 DisplayName FROM NAC_M_UserManagement where UserID=@RequestInsertUserId)
	if(@RequestUserType=1)
		SET @RequestInsertUser = (SELECT TOP 1 Name FROM M_PublicUserProfile where userId_GUID=@RequestInsertUserId)
	if(@RequestUserType=2)
		SET @RequestInsertUser = (SELECT TOP 1 Name FROM M_OrganisationContactPerson where ContactPerson_GUID=@RequestInsertUserId)
	 INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
		 '(' + @InsertColList + ')' + CHAR(10) +  
		 'SELECT ' + @ColList + ',''New'',''Insert'',@RequestInsertUser,CreatedBy,getdate(),''''  FROM INSERTED'
	END
	ELSE
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger  + CHAR(10) + ' INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
		 '(' + @InsertColList + ')' + CHAR(10) +  
		 'SELECT ' + @ColList + ',''New'',''Insert'',NULL,SUSER_SNAME(),getdate(),''''  FROM INSERTED'
	END

 IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

	SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' END'

END

   
IF @GenerateScriptOnly = 1   
BEGIN   
    PRINT REPLICATE ('-',200)   
    PRINT '--Create Script Insert Trigger for ' +  @QuotedSchemaName + '.' + @QuotedTablename   
    PRINT REPLICATE ('-',200)   
    PRINT @SQL   
    PRINT 'GO'   
    PRINT @SQLTrigger   
    PRINT 'GO'   
END   
ELSE   
BEGIN   
    PRINT 'Creating Insert Trigger ' + @QuotedInsertTriggerName + '  for ' + @QuotedSchemaName + '.' + @QuotedTablename   
    EXEC(@SQL)   
    EXEC(@SQLTrigger)   
    PRINT 'Trigger ' + @QuotedSchemaName + '.' + @QuotedInsertTriggerName  + ' Created succesfully'   
END   
   
   
----------------------------------------------------------------------------------------------------------------------   
-- Create Delete Trigger   
----------------------------------------------------------------------------------------------------------------------   
   
   
SELECT @SQL = '   
   
IF EXISTS (SELECT 1    
             FROM sys.objects    
            WHERE Name=''' + @Tablename + '_Delete' + '''   
              AND Schema_id=Schema_id(''' + @Schemaname + ''')   
              AND Type = ''TR'')   
DROP TRIGGER ' + @QuotedSchemaName + '.' + + @QuotedDeleteTriggerName + '   
'   
   
SELECT @SQLTrigger =    
'   
CREATE TRIGGER '  + @QuotedSchemaName + '.'  + @QuotedDeleteTriggerName + '   
ON '+ @QuotedSchemaName + '.' + @QuotedTableName + '   
FOR DELETE   
AS   '

IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' IF SUSER_NAME() NOT IN (''' + @DontAuditforUsersTmp + ''')'
SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' BEGIN'

END

	IF EXISTS( SELECT 1 FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '' + 
	REPLACE(REPLACE(@QuotedTableName,'[',''),']','') + '' AND  COLUMN_NAME = '' + @EserviceId + '')
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' 
		DECLARE @RequestDeletedUser NVARCHAR(225), @DeletedEserviceId NVARCHAR(225),@RequestDeletedUserId NVARCHAR(225)
			SET @DeletedEserviceId = (SELECT TOP 1 ' + @EserviceId + ' FROM DELETED)
			SET @RequestDeletedUser = (SELECT TOP 1 UserName FROM NAC_RequestDeletedUser WHERE EserviceId = @DeletedEserviceId)
			SET @RequestDeletedUserId = (SELECT TOP 1 UserId FROM NAC_RequestDeletedUser WHERE EserviceId = @DeletedEserviceId)
			INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
			'(' + @InsertColList + ')' + CHAR(10) +  
			'SELECT ' + @ColList + ',''Old'',''Delete'',@RequestDeletedUser,@RequestDeletedUserId,getdate(),''''  FROM DELETED' 
	END
	ELSE
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' 
		INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
		'(' + @InsertColList + ')' + CHAR(10) +  
		'SELECT ' + @ColList + ',''Old'',''Delete'',SUSER_SNAME(),NULL,getdate(),''''  FROM DELETED'
	END
	
IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

	SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' END'

END

   
IF @GenerateScriptOnly = 1   
BEGIN   
    PRINT REPLICATE ('-',200)   
    PRINT '--Create Script Delete Trigger for ' + @QuotedSchemaName + '.' + @QuotedTableName   
    PRINT REPLICATE ('-',200)   
    PRINT @SQL   
    PRINT 'GO'   
    PRINT @SQLTrigger   
    PRINT 'GO'   
END   
ELSE   
BEGIN   
    PRINT 'Creating Delete Trigger ' + @QuotedDeleteTriggerName + '  for ' + @QuotedSchemaName + '.' + @QuotedTableName   
    EXEC(@SQL)   
    EXEC(@SQLTrigger)   
    PRINT 'Trigger ' + @QuotedSchemaName + '.' + @QuotedDeleteTriggerName + ' Created succesfully'   
END   
   
----------------------------------------------------------------------------------------------------------------------   
-- Create Update Trigger   
----------------------------------------------------------------------------------------------------------------------   
   
   
SELECT @SQL = '   
   
IF EXISTS (SELECT 1    
             FROM sys.objects    
            WHERE Name=''' + @Tablename + '_Update' + '''   
              AND Schema_id=Schema_id(''' + @Schemaname + ''')   
              AND Type = ''TR'')   
DROP TRIGGER ' + @QuotedSchemaName + '.' + @QuotedUpdateTriggerName + '   
'   
   
SELECT @SQLTrigger =   
'   
CREATE TRIGGER ' + @QuotedSchemaName + '.'  + @QuotedUpdateTriggerName + '     
ON '+ @QuotedSchemaName + '.' + @QuotedTableName + '   
FOR UPDATE   
AS '

IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' IF SUSER_NAME() NOT IN (''' + @DontAuditforUsersTmp + ''')'
SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' BEGIN'

END

IF EXISTS( SELECT 1 FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '' + 
	REPLACE(REPLACE(@QuotedTableName,'[',''),']','') + '' AND  COLUMN_NAME = 'UpdatedBy')
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + '  
		
	--(case when exists (select *
 --                      from INFORMATION_SCHEMA.COLUMNS 
 --                      where TABLE_NAME = '''+@Tablename+''' and
 --                            COLUMN_NAME = ''UserType'' ) then UserType  else 1 end)
			DECLARE @UpdatedCols varchar(max), @RequestUserType int, @RequestInsertUser NVARCHAR(225),@RequestInsertUserId NVARCHAR(225)
	Select @RequestUserType = UserType,@RequestInsertUserId=UpdatedBy FROM INSERTED
	if(@RequestUserType=0)
		SET @RequestInsertUser = (SELECT TOP 1 DisplayName FROM NAC_M_UserManagement where UserID=@RequestInsertUserId)
	if(@RequestUserType=1)
		SET @RequestInsertUser = (SELECT TOP 1 Name FROM M_PublicUserProfile where userId_GUID=@RequestInsertUserId)
	if(@RequestUserType=2)
		SET @RequestInsertUser = (SELECT TOP 1 Name FROM M_OrganisationContactPerson where ContactPerson_GUID=@RequestInsertUserId)
		   SELECT @UpdatedCols = ' + @UpdateCheck + '

		   IF (LTRIM(RTRIM(@UpdatedCols)) <> '''' AND 
		   LTRIM(RTRIM(@UpdatedCols)) <> ''[UpdatedDate]-'' AND
		   LTRIM(RTRIM(@UpdatedCols))<>''[UpdatedBy]-'' AND 
		   LTRIM(RTRIM(@UpdatedCols)) <> ''[UpdatedDate]-[UpdatedBy]-'')
		   BEGIN			
				INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
				 '(' + @InsertColList + ')' + CHAR(10) +  
				 'SELECT ' + @ColList +',''New'',''Update'',@RequestInsertUser,UpdatedBy,getdate(),@UpdatedCols  FROM INSERTED    
   
				  INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
				 '(' + @InsertColList + ')' + CHAR(10) +  
				 'SELECT ' + @ColList +',''Old'',''Update'',@RequestInsertUser,@RequestInsertUserId,getdate(),@UpdatedCols  FROM DELETED 
		   END'
	END
	ELSE
	BEGIN
		SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + '  

			DECLARE @UpdatedCols varchar(max)

		   SELECT @UpdatedCols = ' + @UpdateCheck + '

		   IF (LTRIM(RTRIM(@UpdatedCols)) <> '''' AND 
		   LTRIM(RTRIM(@UpdatedCols)) <> ''[UpdatedDate]-'' AND
		   LTRIM(RTRIM(@UpdatedCols))<>''[UpdatedBy]-'' AND 
		   LTRIM(RTRIM(@UpdatedCols)) <> ''[UpdatedDate]-[UpdatedBy]-'')
		   BEGIN			
				INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
				 '(' + @InsertColList + ')' + CHAR(10) +  
				 'SELECT ' + @ColList +',''New'',''Update'',SUSER_SNAME(),NULL,getdate(),@UpdatedCols  FROM INSERTED    
   
				  INSERT INTO ' + @QuotedSchemaName + '.' + @QuotedAuditTableName + CHAR(10) +  
				 '(' + @InsertColList + ')' + CHAR(10) +  
				 'SELECT ' + @ColList +',''Old'',''Update'',SUSER_SNAME(),NULL,getdate(),@UpdatedCols  FROM DELETED 
		   END'
	END

IF LTRIM(RTRIM(@DontAuditforUsersTmp)) <> '' 
BEGIN

	SELECT @SQLTrigger = @SQLTrigger + CHAR(10) + ' END'

END


   
IF @GenerateScriptOnly = 1   
BEGIN   
    PRINT REPLICATE ('-',200)   
    PRINT '--Create Script Update Trigger for ' + @QuotedSchemaName + '.' + @QuotedTableName   
    PRINT REPLICATE ('-',200)   
    PRINT @SQL   
    PRINT 'GO'   
    PRINT @SQLTrigger   
    PRINT 'GO'   
END   
ELSE   
BEGIN   
    PRINT 'Creating Delete Trigger ' + @QuotedUpdateTriggerName + '  for ' + @QuotedSchemaName + '.' + @QuotedTableName   
    EXEC(@SQL)   
    EXEC(@SQLTrigger)   
    PRINT 'Trigger ' + @QuotedSchemaName + '.' + @QuotedUpdateTriggerName + '  Created succesfully'   
END   
   
SET NOCOUNT OFF   
   



GO

