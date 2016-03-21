module User
where

import Database.HDBC
import Database.HDBC.Sqlite3
import DatabaseConnection (withConnection)

isValid :: String -> String -> IO [[String]]
isValid username password = do
    withConnection $ \conn -> do
        r <- quickQuery' conn
            "select * from users where username=? and password=?"
            [toSql username, toSql password]
        return $ map (map fromSql) r
