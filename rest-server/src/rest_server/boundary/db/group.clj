(ns rest-server.boundary.db.group
  (:require [duct.database.sql]
            [honeysql.core :as sql]
            [rest-server.boundary.db.core :as db]))

(defprotocol Groups
  (list-groups [db])
  (create-group [db group])
  (fetch-group [db group-id])
  (fetch-group-admin-members [db group-id])
  (create-group-members [db group-members]))

(extend-protocol Groups
  duct.database.sql.Boundary
  (list-groups [db]
    (db/select db (sql/build :select :*
                             :from :groups)))
  (create-group [db group]
    (db/insert! db :groups group))
  (fetch-group [db group-id]
    (db/select-one db (sql/build :select :*
                                 :from :groups
                                 :where [:= :id group-id])))
  (fetch-group-admin-members [db group-id]
    (db/select db (sql/build :select :members.*
                             :from :members
                             :join [:groups_members [:= :members.id :groups_members.member_id]]
                             :where [:and
                                     [:= :groups_members.group_id group-id]
                                     [:= :groups_members.admin true]])))
  (create-group-members [db group-members]
    (db/insert-multi! db :groups_members group-members)))
