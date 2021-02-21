(defn wrap-subscriber
  [^HttpResponse$BodySubscriber subscriber
   {:keys [coerce-response]}]

  (reify HttpResponse$BodyHandler
    (apply [this resp-info]

      (let [status (.statusCode resp-info)]
        (case coerce-response

          (nil false :none :never)
          (HttpResponse$BodySubscribers/discarding)

          :positive-only
          (if (good-status? status)
            subscriber
            (HttpResponse$BodySubscribers/discarding))

          :negative-only
          (if (good-status? status)
            (HttpResponse$BodySubscribers/discarding)
            subscriber)

          (:always true)
          subscriber

          ;; else
          (throw (ex-info "wrong coerce-resp")))))))


    ;; path,
    ;; open-option...
    ;; https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/StandardOpenOption.html
    ;; :file
    ;; (HttpResponse$BodyHandlers/ofFile "aaa")


    ;; ofFileDownload
    ;; open-option...
    ;; (HttpResponse$BodyHandlers/ofFile path)


    ;; (file? body)
    ;; (HttpRequest$BodyPublishers/ofFile body) ;; wrap file
