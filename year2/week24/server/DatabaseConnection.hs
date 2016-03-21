module DatabaseConnection
(withConnection)
where

import Database.HDBC
import Database.HDBC.Sqlite3

withConnection :: (Connection -> IO a) -> IO a
withConnection f = do
    conn <- connectSqlite3 "db.db"
    r <- f conn
    commit conn
    disconnect conn
    return r
