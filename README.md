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

* Create a database
* Create a login account on the database

* Execute sql/01_create_Table.sql on the database
* Execute sql/02_create_SP.sql on the database
* Execute sql/03_insert.sql on the database

* Adjust ```ConnectInfo``` in app/Main.hs to make suitable for the login account


### Run example

Run example with REPL like below.
```
$ stack repl

*Main> :main

```

