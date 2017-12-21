(ns rest-server.boundary.member
  (:require [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defprotocol Members
  (list-members [db])
  (create-member [db member])
  (fetch-member [db member-id])
  (fetch-members [db member-ids]))

(extend-protocol Members
  duct.database.sql.Boundary
  (list-members [{db :spec}]
    (->> (sql/build :select :*
                    :from :members)
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-member [{db :spec} member]
    (->> member
         util/transform-keys-to-snake
         (jdbc/insert! db :members)
         ffirst
         val))
  (fetch-member [{db :spec} member-id]
    (->> (sql/build :select :*
                    :from :members
                    :where [:= :id member-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab
         first))
  (fetch-members [{db :spec} member-ids]
    (->> (sql/build :select :*
                    :from :members
                    :where [:in :id member-ids])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab)))
