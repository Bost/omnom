(ns omnom.issue823
  (:require [devcards.core :refer-macros [defcard deftest dom-node]]
            [clojure.string :as string]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defui ^:once ChildOne
  static om/Ident (ident [_ {:keys [db/id]}] [:db/id id])
  static om/IQuery (query [_] [:db/id :doc/relevant-to-child-one])
  Object
  (render [this]
    (dom/h2 nil
      (str "Child One: " (-> this om/props :doc/relevant-to-child-one)))))

(def child-one (om/factory ChildOne))

(defui ^:once ChildTwo
  static om/Ident (ident [_ {:keys [db/id]}] [:db/id id])
  static om/IQuery (query [_] [:db/id :doc/relevant-to-child-two])
  Object
  (render [this]
    (dom/h2 nil
      (str "Child Two: " (-> this om/props :doc/relevant-to-child-two)))))

(def child-two (om/factory ChildTwo))

(defui ^:once Parent
  static om/IQuery
  (query [_]
    [;; What about ChildTwo?? (om/get-query ChildTwo)
     ;; Queries will overwrite each other
     {:app/doc (om/get-query ChildOne)}
     #_{:app/doc (om/get-query ChildTwo)}
     ])
  Object
  (render [this]
    (let [props          (om/props this)
          children-props (:app/doc props)]
      (println "props" props)
      (println "children-props" children-props)
      (dom/div nil
        (child-one children-props)
        (child-two children-props)))))

(def state
  {:app/doc
   {:db/id                     1
    :doc/relevant-to-child-one "Child One Info"
    :doc/relevant-to-child-two "Child Two Info"}})

(def parser
  (om/parser
   {:read
    (fn [{:keys [state query] :as env} k _]
      (let [st @state]
        {:value (om/db->tree query (get st k) st)}))}))

(def reconciler
  (om/reconciler {:state  state :parser parser}))

(defcard test-children
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler Parent node))))
