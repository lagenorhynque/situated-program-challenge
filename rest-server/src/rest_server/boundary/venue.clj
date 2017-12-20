(ns rest-server.boundary.venue
  (:require [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defprotocol Venues
  (list-venues [db group-id])
  (create-venue [db member]))

(extend-protocol Venues
  duct.database.sql.Boundary
  (list-venues [{db :spec} group-id]
    (->> (sql/build :select :*
                    :from :venues
                    :where [:= :group_id group-id])
         sql/format
         (jdbc/query db)
         (map util/transform-keys-to-kebab)))
  (create-venue [{db :spec} venue]
    (->> venue
         util/transform-keys-to-snake
         (jdbc/insert! db :venues)
         ffirst
         val)))
