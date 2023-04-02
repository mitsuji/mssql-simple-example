# mssql-simple-example: Usage example of mssql-simple

## Related projects

* [mssql-simple](https://github.com/mitsuji/mssql-simple)
  : SQL Server client library implemented in Haskell  
  https://github.com/mitsuji/mssql-simple
  
* [ms-tds](https://github.com/mitsuji/ms-tds)
  : TDS Protocol implemented in Haskell  
  https://github.com/mitsuji/ms-tds
  

## How to

### Prepare stack environment

Download and install [stack](https://docs.haskellstack.org/en/stable/README/).


### Prepare example database

Setup example database by using scripts in sql directory.

* Setup some kind of SQL Server environment.  
  Using [docker image](https://learn.microsoft.com/en-us/sql/linux/quickstart-install-connect-docker) would be a convinient chose.

* Execute sql/00_setup_Database.sql on the master database.
* Execute sql/01_create_Table.sql on the mss-test database.
* Execute sql/02_create_SP.sql on the mss-test database.
* Execute sql/03_insert.sql on the mss-test database.


### Run example

Run example with REPL like below.
```
$ stack repl

*Main> :main

```

