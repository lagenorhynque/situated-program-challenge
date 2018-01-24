(ns rest-server.util
  (:require [camel-snake-kebab.core :refer [->kebab-case ->snake_case]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.string :as str])
  (:import (java.sql Timestamp)
           (java.time Instant)
           (org.postgresql.util PGobject)))

(defn transform-keys-to-kebab [m]
  (transform-keys #(->kebab-case % :separator \_) m))

(defn transform-keys-to-snake [m]
  (transform-keys #(->snake_case % :separator \-) m))

(defn kw->pgenum [enum-kw]
  (doto (PGobject.)
    (.setType (-> enum-kw
                  namespace
                  (str/replace \- \_)))
    (.setValue (name enum-kw))))

(defn string->timestamp [s]
  (some-> s
          Instant/parse
          Timestamp/from))

(defn now []
  (Timestamp/from (Instant/now)))
