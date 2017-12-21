(ns rest-server.boundary.venue
  (:require [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defprotocol Venues
  (list-venues [db group-id])
  (create-venue [db member])
  (fetch-venue [db venue-id]))

(extend-protocol Venues
  duct.database.sql.Boundary
  (list-venues [{db :spec} group-id]
    (->> (sql/build :select :*
                    :from :venues
                    :where [:= :group_id group-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-venue [{db :spec} venue]
    (->> venue
         util/transform-keys-to-snake
         (jdbc/insert! db :venues)
         ffirst
         val))
  (fetch-venue [{db :spec} venue-id]
    (->> (sql/build :select :*
                    :from :venues
                    :where [:= :id venue-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab
         first)))
