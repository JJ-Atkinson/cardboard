(ns cardboard.demo-ws
  (:require #_["react-monaco-editor"]
    [fulcro.client.primitives :as fp]
    [fulcro.client.localized-dom :as dom]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro-css.css :as css]
    [fulcro.client.mutations :as fm]
    [fulcro.client.primitives :as prim]
    [cardboard.ui.editor :as editor]))

(fp/defsc FulcroDemo
  [this {:keys [counter]}]
  {:initial-state (fn [_] {:counter 0})
   :ident         (fn [] [::id "singleton"])
   :query         [:counter]}
  (dom/div
    (str "Hey Eric  [" counter "]")
    (dom/button {:onClick #(fm/set-value! this :counter (inc counter))} "+")))


(fp/defsc DemoCard
  [this {:keys [text textbox?]}]
  {:initial-state (fn [_] {:text     "Hi world"
                           :textbox? true})
   :ident         (fn [] [::id "singleton"])
   :query         [:text :textbox?]}
  (dom/div
    (str "fcdm [" text "]")
    (dom/br)
    (dom/button {:onClick #(fm/set-value! this :textbox? (not textbox?))} "Show/Hide")
    (dom/br)
    (when textbox?
      (dom/textarea {:defaultValue text :onChange #(fm/set-string! this :text :event %)}))))

(defn js-e-pos [evt]
  {:x (.-clientX evt)
   :y (.-clientY evt)})
(defn pos-combine [f m1 m2]
  (merge-with f m1 m2))


;(def floating-css [[:.draggable-component {:background-color "#eee"
;                                           :position         :absolute}]
;                   [:.title-bar {:width            "100%"
;                                 :background-color :black}]])
;
;(fp/defsc FloatingTB
;  [this {:Draggable/keys [id loc last-client-pos dragging?]}]
;
;  {:initial-state (fn [_] {
;                           :Draggable/loc             {:x 0 :y 0}
;                           :Draggable/last-client-pos nil
;                           :Draggable/dragging?       false})
;   
;   :ident         [:Draggable/by-id :db/id]
;   
;   :query         [:db/id :Draggable/loc :Draggable/last-client-pos :Draggable/dragging?]
;   :css           floating-css}
;  (dom/div "greetings") 
;  
;  (dom/div :.draggable-component
;           {:style {:left (:x loc)
;                    :top  (:y loc)}}
;           (letfn [(update-loc [e] (fm/set-value! this :Draggable/loc
;                                                  (pos-combine + loc (pos-combine - (js-e-pos e) last-client-pos))))
;                   (update-last-cp [e] (fm/set-value! this :Draggable/last-client-pos (js-e-pos e)))
;                   (update-dragging [val] (fm/set-value! this :Draggable/dragging? val))]
;             (dom/div :.title-bar
;                      {:onMouseMove (fn [e] (when dragging? (update-loc e) (update-last-cp e)))
;                       :onMouseDown (fn [e] (update-last-cp e) (update-dragging true))
;                       :onMouseUp   (fn [e] (update-dragging false))}
;                      "_"))
;           (dom/span (str "Demo of Draggable component {:x " (:x loc) ", :y " (:y loc) "}"))))







(ws/defcard fulcro-demo-card
            (ct.fulcro/fulcro-card
              {::f.portal/root FulcroDemo}))

(ws/defcard fulcro-demo-card-mine
            (ct.fulcro/fulcro-card
              {::f.portal/root DemoCard}))

;(ws/defcard dcard-floating-TB
;            (ct.fulcro/fulcro-card
;              {::f.portal/root FloatingTB}))


(ws/defcard single-editor
            (ct.fulcro/fulcro-card
              {::f.portal/root          editor/Editor
               ::f.portal/initial-state {:id 1 :content "(defn ...)"}}))

(ws/defcard editor-workspace
            (ct.fulcro/fulcro-card
              {::f.portal/root          editor/EditorWorkspace
               ::f.portal/initial-state {:editors (mapv #(prim/get-initial-state editor/Editor %)
                                                        [{:id 1 :content "(defn 1...)"}
                                                         {:id 2 :content "(defn 2...)"}
                                                         {:id 3 :content "(defn 3...)"}
                                                         {:id 4 :content "(defn 4...)"}])}
               ::f.portal/wrap-root? true}))
