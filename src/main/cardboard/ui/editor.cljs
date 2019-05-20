(ns cardboard.ui.editor
  (:require #_["react-monaco-editor"]
    [fulcro.client.primitives :as fp]
    [fulcro.client.localized-dom :as dom]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro-css.css :as css]
    [fulcro.client.mutations :as fm]
    [fulcro.client.primitives :as prim]
    [fulcro.client.data-fetch :as df]))


(def editor-css [[:.editor-textarea {:background-color "#eee"}]
                 [:.title-bar {:width            "100%"
                               :background-color :black}]])



(fm/defmutation update-editor-content
  [{:keys [editor-id new-content]}]
  (action [{:keys [state]}]
          (do (println "updating with new-content" new-content "on id" editor-id)
              (swap! state assoc-in [:Editor/id editor-id :Editor/content] new-content))))


(fp/defsc Editor [this {:Editor/keys [id content] :as props}]
  {:initial-state (fn [{:keys [id content]}] {:Editor/id id :Editor/content content})
   :query         [:Editor/id :Editor/content]
   :ident         [:Editor/id :Editor/id]
   :css           [[:.editor-textarea {:background-color "#eee"
                                       :resize           :none}]]}
  (do
    (println "editor content=" content "id=" id "allprops=" props)
    (dom/textarea :.editor-textarea
                  {:value    (if content content "")
                   :onChange (fn [e] (println (.. e -target -value))
                               (prim/transact! this `[(update-editor-content
                                                        {:editor-id ~id :new-content ~(.. e -target -value)})]))
                   :onClick  (fn [e] (println "clicked" id)
                               (df/load this [:Editor/id id] Editor))
                   :rows     10
                   :cols     60
                   :key      id})))

(fm/defmutation add-editor
  [_]
  (action [{:keys [state]}]
          (let [id (->> @state :Editor/id keys (apply max) inc)]
            (println "adding an editor on id" id)
            (swap! state assoc-in [:Editor/id id] (prim/get-initial-state Editor {:id id :content ""})))))

(def ui-editor (prim/factory Editor {:keyfn :Editor/id}))

(fp/defsc EditorWorkspace [this {:EditorWorkspace/keys [id editors]}]
  {:initial-state (fn [{:keys [editors]}] {:EditorWorkspace/editors editors})
   :query         [:db/id {:EditorWorkspace/editors (prim/get-query Editor)}]}
  (println "editors=" editors)
  (dom/div :#me (map ui-editor editors)))