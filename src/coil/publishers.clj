(ns coil.publishers
  (:import
   java.util.function.Supplier
   java.net.http.HttpRequest$BodyPublishers))


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
