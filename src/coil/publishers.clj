(ns coil.publishers
  (:import
   java.util.function.Supplier
   java.net.http.HttpRequest$BodyPublishers))


(defn of-byte-array
  ([bytes]
   (HttpRequest$BodyPublishers/ofByteArray bytes))
  ([bytes offset length]
   (HttpRequest$BodyPublishers/ofByteArray bytes offset length)))


(defn fn->of-input-stream [stream-fn]
  (HttpRequest$BodyPublishers/ofInputStream
   (reify Supplier
     (get [_]
       (stream-fn)))))


(defn stream->of-input-stream
  [stream]
  (let [called? (atom nil)]
    (HttpRequest$BodyPublishers/ofInputStream
     (reify Supplier
       (get [_]
         (when-not @called?
           (reset! called? true)
           stream))))))


(defn no-body []
  (HttpRequest$BodyPublishers/noBody))

(defn of-string
  ([string]
   (HttpRequest$BodyPublishers/ofString string))
  ([string charset]
   (HttpRequest$BodyPublishers/ofString string charset)))
