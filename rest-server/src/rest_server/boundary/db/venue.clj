(ns rest-server.boundary.db.venue
  (:require [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.boundary.db.core :as db]))

(defprotocol Venues
  (list-venues [db group-id])
  (create-venue [db member])
  (fetch-venue [db venue-id]))

(extend-protocol Venues
  duct.database.sql.Boundary
  (list-venues [db group-id]
    (db/select db (sql/build :select :*
                             :from :venues
                             :where [:= :group_id group-id])))
  (create-venue [db venue]
    (db/insert! db :venues venue))
  (fetch-venue [db venue-id]
    (db/select-one db (sql/build :select :*
                                 :from :venues
                                 :where [:= :id venue-id]))))
