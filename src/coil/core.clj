(ns coil.core
  (:import
   java.net.URI

   java.net.http.HttpClient
   java.net.http.HttpClient$Redirect
   java.net.http.HttpClient$Version
   java.net.http.HttpClient$Version
   java.net.http.HttpHeaders
   java.net.http.HttpRequest
   java.net.http.HttpRequest$BodyPublisher
   java.net.http.HttpRequest$BodyPublishers
   java.net.http.HttpRequest$Builder
   java.net.http.HttpResponse
   java.net.http.HttpResponse$BodyHandler
   java.net.http.HttpResponse$BodyHandlers
   java.net.http.HttpResponse$BodySubscribers
   java.net.http.HttpResponse$BodySubscribers
   java.net.http.HttpResponse$ResponseInfo

   java.time.Duration)
  (:require
   [coil.mults :as m]
   [coil.query :as q]
   [coil.auth :as a]
   [coil.util :as u]
   [coil.publishers :as pub]

   coil.edn
   coil.reader
   coil.json

   [clojure.string :as str]))


(defprotocol IClojure
  (->clj [value]))


(extend-protocol IClojure

  HttpHeaders
  (->clj [headers]
    (reduce
     (fn [result [header values]]
       (assoc result (keyword header) (first values)))
     {}
     (.map headers)))

  HttpResponse
  (->clj [response]
    {:status (.statusCode response)
     :headers (-> response .headers ->clj)
     :url (-> response .uri str)
     :version (-> response .version ->clj)
     :body (.body response)})

  HttpClient$Version
  (->clj [version]
    (case (str version)
      "HTTP_1_1" :http-1.1
      "HTTP_2"   :http-2)))


(defn ->redirect [kword]
  (case kword
    :always
    HttpClient$Redirect/ALWAYS
    :never
    HttpClient$Redirect/NEVER
    :normal
    HttpClient$Redirect/NORMAL))


(defn ->version [kword]
  (case kword
    :http-1.1
    HttpClient$Version/HTTP_1_1
    :http-2
    HttpClient$Version/HTTP_2))


(defn ^HttpClient make-client

  [{:as opt
    :keys [version
           redirect]}]

  (cond-> (HttpClient/newBuilder)

    redirect
    (.followRedirects (->redirect redirect))

    version
    (.version (->version redirect))

    true
    .build)

  ;;
  ;; connectTimeout
  ;; proxy
  ;; authenticator
  ;;

  )


(defn ^java.net.http.HttpRequest$BodyPublisher
  make-body-publisher
  [{:as opt :keys [body]}]
  (cond

    ;; offset length
    (bytes? body)
    (HttpRequest$BodyPublishers/ofByteArray body)


    (fn? body)
    (pub/fn->of-input-stream body)

    (u/input-stream? body)
    (pub/stream->of-input-stream body)

    ;; charset
    (string? body)
    (HttpRequest$BodyPublishers/ofString body)

    (nil? body)
    (HttpRequest$BodyPublishers/noBody)

    :else
    (m/make-custom-body-publisher opt)))


(defn ^HttpRequest$Builder
  set-method-&-publisher
  [^HttpRequest$Builder builder
   {:as opt :keys [method]}]

  (let [publisher (make-body-publisher opt)
        java-method (-> method name str/upper-case)]
    (.method builder java-method publisher)))


(defn set-headers
  [^HttpRequest$Builder builder headers]
  (doseq [[header value] headers]
    (.header builder (name header) (str value)))
  builder)


(defn set-timeout
  [^HttpRequest$Builder builder timeout]
  (.timeout builder (java.time.Duration/ofMillis timeout)))


(defn set-expect-continue
  [^HttpRequest$Builder builder expect-flag]
  (.expectContinue builder expect-flag))


(defn set-url
  [^HttpRequest$Builder builder
   {:keys [url query-params]}]
  (let [url
        (cond-> url
          query-params
          (str "?" (q/make-query-string query-params)))]
    (.uri builder (new URI url))))


(defn build-request
  [^HttpRequest$Builder builder]
  (.build builder))


(defn ^HttpRequest
  make-request [opt]

  (assert (:url opt) "URL not set")
  (assert (:method opt) "HTTP method not set")

  (let [{:keys [basic-auth]} opt

        opt (-> opt
                m/handle-content-type
                a/handle-basic-auth)

        {:keys [url
                timeout
                headers
                expect-continue?]} opt

        builder
        (-> (HttpRequest/newBuilder)
            (set-url opt)
            (set-method-&-publisher opt))]

    (cond-> builder

      headers
      (set-headers headers)

      timeout
      (set-timeout timeout)

      expect-continue?
      (set-expect-continue expect-continue?)

      true
      build-request)))


(defn ^HttpResponse$BodyHandlers
  make-body-handler
  [{:keys [as
           response-charset]}]

  (case as

    :bytes
    (HttpResponse$BodyHandlers/ofByteArray)

    :stream
    (HttpResponse$BodyHandlers/ofInputStream)

    :lines
    (HttpResponse$BodyHandlers/ofLines)

    :string
    (if response-charset
      (HttpResponse$BodyHandlers/ofString response-charset)
      (HttpResponse$BodyHandlers/ofString))

    (nil false :drop :none :skip :discard)
    (HttpResponse$BodyHandlers/discarding)

    ;; else
    (HttpResponse$BodyHandlers/ofInputStream)))


(defn handle-exceptions
  [{:as response :keys [status]}
   {:keys [throw-exceptions]}]

  (if (and throw-exceptions
           (not (u/positive-status? status)))

    (throw (ex-info "Error"
                    (assoc response :type ::bad-status)))

    response))


(def opt-default
  {:method :get
   :as :stream
   :redirect :always
   :version :http-1.1
   ;;

   })





;; http://catalog.data.gov/api/3/sdfsf
;; cors
;; content-type
;; content-length
;; date
;; other date headers (cache)
;; cookies?
(defn parse-headers [])


;; ssl certs support
;; yaml support

;; publishers ns
;; handlers ns


;; multi-project repo
;; readme


{:basic-auth ["sdfds" "sdfsdf"]}

(defn request

  ([opt]

   (let [opt (merge opt-default opt)
         client (make-client opt)]
     (request client opt)))

  ([^HttpClient client opt]

   (let [opt (merge opt-default opt)
         req (make-request opt)
         handler (make-body-handler opt)

         {:keys [async?]} opt

         post-fn
         (fn [response]
           (-> response
               ->clj
               (m/handle-as opt)
               (handle-exceptions opt)))]

     (if async?

       (-> (.sendAsync client req handler)
           (.thenApply (u/->function post-fn)))

       (-> (.send client req handler)
           post-fn)))))
