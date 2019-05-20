(ns cardboard.model.user
  (:require
    [com.wsscode.pathom.connect :as pc]
    [fulcro.server :refer [defquery-root defquery-entity]]
    [cardboard.server-components.pathom-wrappers :refer [defmutation defresolver]]
    [taoensso.timbre :as log]
    [datoteka.core :as fs]
    [cardboard.model.src-dir-manager :as sdm]))

(def dev-test-path (fs/path "/media/jarrett/Windows/code-projects/electron-apps/reframe-transform/src"))

(def user-database (atom {}))
(def editor-database (atom (-> dev-test-path
                               sdm/blocks-in-dir
                               (sdm/by-unique-key :id))))

(defresolver all-users-resolver
  "Resolve queries for :all-users."
  [env input]
  {;;GIVEN nothing
   ::pc/output [{:all-users [:user/id]}]}                   ;; I can output all users. NOTE: only ID is needed...other resolvers resolve the rest
  (log/info "All users. Database contains: " @user-database)
  {:all-users (mapv
                (fn [id] {:user/id id})
                (keys @user-database))})

;(defquery-root :Editor    
;  "Resolve a single editor :INFO testing rn"
;  [env {:keys [db/id]}]
;  {::pc/intput #{:db/id}
;   ::pc/output [:Editor/content]}
;  (log/info "Trying to resolve a single editor with id" id)
;  {:Editor/content (str "This is server code for ya bud. {:Editor/id " id "}")})

(defresolver econtent-resolver
  "Resolves the content of an editor."
  [env {:Editor/keys [id]}]
  {::pc/input  #{:Editor/id}
   ::pc/output [:Editor/content]}
  {:Editor/content (let [cb (get @editor-database id)
                         cb-code-resolved (->> cb :contents deref :code)]
                     (println "called" id "env")
                     (println "db cb" cb)
                     (println "db cb resolved")
                     (clojure.pprint/pprint cb-code-resolved)
                     cb-code-resolved)})



(defresolver user-resolver
  "Resolve details of a single user.  (See pathom docs for adding batching)"
  [env {:user/keys [id]}]
  {::pc/input  #{:user/id}                                  ; GIVEN a user ID
   ::pc/output [:user/name]}                                ; I can produce a user's details
  ;; Look up the user (e.g. in a database), and return what you promised
  (when (contains? @user-database id)
    (get @user-database id)))

(defresolver user-address-resolver
  "Resolve address details for a user.  Note the address data could be stored on the user in the database or elsewhere."
  [env {:user/keys [id]}]
  {::pc/input  #{:user/id}                                  ; GIVEN a user ID
   ::pc/output [:address/id :address/street :address/city :address/state :address/postal-code]} ; I can produce address details
  (log/info "Resolving address for " id)
  {:address/id          "fake-id"
   :address/street      "111 Main St."
   :address/city        "Nowhere"
   :address/state       "WI"
   :address/postal-code "99999"})

(defmutation upsert-user
             "Add/save a user. Required parameters are:
           
             :user/id - The ID of the user
             :user/name - The name of the user
           
             Returns a User (e.g. :user/id) which can resolve to a mutation join return graph.
             "
             [{:keys [config ring/request]} {:user/keys [id name]}]
             {::pc/params #{:user/id :user/name}
              ::pc/output [:user/id]}
             (log/debug "Upsert user with server config that has keys: " (keys config))
             (log/debug "Ring request that has keys: " (keys request))
             (when (and id name)
               (swap! user-database assoc id {:user/id   id
                                              :user/name name})
               ;; Returning the user id allows the UI to query for the result. In this case we're "virtually" adding an address for
               ;; them!
               {:user/id id}))
