(ns cardboard.ui.root
  (:require
    [fulcro.client.dom :as dom :refer [div ul li p h3]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :as fm]
    [cardboard.model.user :as user]
    [cardboard.ui.components :as comp]
    [cardboard.ui.editor :as e]
    [taoensso.timbre :as log]))

(defsc User [this {:user/keys    [name]
                   :address/keys [street city]}]
  {:query [:user/id :user/name :address/street :address/city]
   :ident [:user/id :user/id]}
  (li :.ui.item
      (div :.content
           (println "user query is " (prim/get-query User))
           (str name (when street (str " of " street ", " city))))))


(defsc Draggable [this {:draggable/keys [loc last-pos dragging?]}]

  {:initial-state (fn [_] {:draggable/loc       {:x 0 :y 0}
                           :draggable/last-pos  {:x 0 :y 0}
                           :draggable/dragging? false})
   ;:ident         (fn [] [:draggable/id :draggable/id])
   :query         [:draggable/loc :draggable/last-pos :draggable/dragging?]}
  (dom/div
    (dom/div {:onMouseMove (fn [e] (println "mm" (.-clientY e)))
              :onMouseDown (fn [e] (println "md" (.-clientX e)))
              :onMouseUp   (fn [e] (println "mu" (.-clientX e)))}
             "__________________")
    (dom/span (str "Fulcro counter demo [" "]"))
    (dom/button {:onClick #(fm/set-value! this :draggable/loc nil)} "+")))



(def ui-user (prim/factory User {:keyfn :user/id}))

(def draggable (prim/factory Draggable {:keyfn :draggable/id}))

(defsc Root [this {:keys [all-users all-editors]}]
  {:query         [{:all-users (prim/get-query User)}
                   {:all-editors (prim/get-query e/Editor)}]
   :initial-state (fn [{:keys [contents] :or {contents "default contents from init state"}}]
                    {:all-users   []
                     :all-editors [{:Editor/id 1 :Editor/content "Editor 1 default contents"}
                                   {:Editor/id 2 :Editor/content contents}
                                   #_(prim/get-initial-state e/Editor {:id 1 :content "Hello world"})]})}
  (div :.ui.segments
       (div :.ui.segment
            #_(draggable)
            "Editor-list"
            (dom/br)
            (div :.editors
                 (map e/ui-editor all-editors))
            (dom/br)
            "Text contents of editor query result"
            (str all-editors)
            (dom/br)
            "Text contents of user query result"
            (str all-users)
            (dom/br)
            "Sample editor w/ static content"
            (dom/br)
            (e/ui-editor {:Editor/id 1 :Editor/content "From Static args"}) ; just to give it an id for the server trials
            )

       (div :.ui.segment
            (div :.content
                 (div "Your system has the following users in the database:")
                 (ul :.ui.list
                     (map ui-user all-users)))

            (dom/button :.ui.icon.button
                        {:onClick (fn []
                                    (let [id (str (random-uuid))]
                                      (log/info "Adding user")
                                      ;; NOTE: The lack of quoting works because we're using declare-mutation from incubator. see model.cljs
                                      ;; NOTE 2: This is a "mutation join".  The mutation is sent with a query, and on success it
                                      ;; returns the user.  This allows the server to tack on an address without us specifying it,
                                      ;; and also have the local database/ui update with it.
                                      (prim/transact! this [{(user/upsert-user {:user/id   id
                                                                                :user/name (str "User " id)})
                                                             (prim/get-query User)}])))}
                        (dom/i :.plus.icon)
                        "Add User"))))
