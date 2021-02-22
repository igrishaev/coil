(ns coil.util
  (:import
   java.nio.file.Path
   java.nio.file.Paths

   java.util.function.Supplier

   java.io.File
   java.io.InputStream

   java.util.function.Function

   )
  )


(def positive-statuses
  #{200 201 202 203 204 205 206 207
    300 301 302 303 304 307})


(defn positive-status? [status]
  (contains? positive-statuses status))


(defn ^Path ->path [str-path]
  (Paths/get str-path))


(defmacro ->supplier [val]
  `(reify Supplier
     (get [this]
       ~val)))


(defn deep-merge
  ;; https://gist.github.com/danielpcox/c70a8aa2c36766200a95
  [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))


(defn map->seq [map]
  (mapcat seq map))


(defn ^Function
  ->function
  [f]
  (reify Function
    (apply [this arg] (f arg))))


(def file? (partial instance? java.io.File))

(def input-stream? (partial instance? InputStream))


(defn content-type-matches? [response re]
  (some->> response
           :headers
           :content-type
           (re-find re)
           some?))
