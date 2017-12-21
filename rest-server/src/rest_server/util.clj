(ns rest-server.util
  (:require [camel-snake-kebab.core :refer [->kebab-case ->snake_case]]
            [camel-snake-kebab.extras :refer [transform-keys]])
  (:import (java.sql Timestamp)
           (java.time Instant)))

(defn transform-keys-to-kebab [m]
  (transform-keys #(->kebab-case % :separator \_) m))

(defn transform-keys-to-snake [m]
  (transform-keys #(->snake_case % :separator \-) m))

(defn string->timestamp [s]
  (-> s
      Instant/parse
      Timestamp/from))

(defn now []
  (Timestamp/from (Instant/now)))
