(ns rest-server.boundary.group
  (:require [clojure.java.jdbc :as jdbc]
            [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.util :as util]))

(defprotocol Groups
  (list-groups [db])
  (create-group [db group])
  (fetch-group [db group-id])
  (fetch-group-admin-members [db group-id])
  (create-group-members [db group-members]))

(extend-protocol Groups
  duct.database.sql.Boundary
  (list-groups [{db :spec}]
    (->> (sql/build :select :*
                    :from :groups)
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-group [{db :spec} group]
    (->> group
         util/transform-keys-to-snake
         (jdbc/insert! db :groups)
         ffirst
         val))
  (fetch-group [{db :spec} group-id]
    (->> (sql/build :select :*
                    :from :groups
                    :where [:= :id group-id])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab
         first))
  (fetch-group-admin-members [{db :spec} group-id]
    (->> (sql/build :select :members.*
                    :from :members
                    :join [:groups_members [:= :members.id :groups_members.member_id]]
                    :where [:and
                            [:= :groups_members.group_id group-id]
                            [:= :groups_members.admin true]])
         sql/format
         (jdbc/query db)
         util/transform-keys-to-kebab))
  (create-group-members [{db :spec} group-members]
    (->> group-members
         util/transform-keys-to-snake
         (jdbc/insert-multi! db :groups_members))))
