(ns rest-server.boundary.member
  (:require [camel-snake-kebab.core :refer [->kebab-case ->snake_case]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]))

(defprotocol Members
  (list-members [db])
  (create-member [db member])
  (fetch-member [db member-id]))

(extend-protocol Members
  duct.database.sql.Boundary
  (list-members [{db :spec}]
    (->> (sql/build :select :*
                    :from :members)
         sql/format
         (jdbc/query db)
         (map #(transform-keys ->kebab-case %))))
  (create-member [{db :spec} member]
    (->> member
         (transform-keys ->snake_case)
         (jdbc/insert! db :members)
         ffirst
         val))
  (fetch-member [{db :spec} member-id]
    (->> (sql/build :select :*
                    :from :members
                    :where [:= :id member-id])
         sql/format
         (jdbc/query db)
         (transform-keys ->kebab-case)
         first)))
