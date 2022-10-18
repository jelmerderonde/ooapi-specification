(ns script
  (:require
   [clojure.java.io :as io]
   [clj-yaml.core :as yaml]
   [clojure.walk :as walk]
   [clojure.string :as str]
   [clojure.set :as set]
   [babashka.fs :as fs]))

(def spec-file (io/file "spec.yaml"))

(defn slurp-yaml
  [path]
  (yaml/parse-string (slurp path)
                     :keywords false))

;;(def top-level-spec (slurp-yaml spec-file))

(defn ref?
  [form]
  (and (map? form)
       (contains? form "$ref")))

(defn attach-path-if-ref
  [path form]
  (if (and path (ref? form))
    (assoc form :path path)
    form))

(defn relative-to-file
  [file-path path]
  (if file-path
    (let [current-folder (fs/parent file-path)]
      (loop [result (vec (fs/components current-folder))
             segments (vec (fs/components path))]
        (let [[segment & segments] segments]
          (if segment
            (recur
             (case (str segment)
               "." result
               ".." (vec (butlast result))
               (conj result segment))
             segments)
            (str (apply fs/path result))))))
    path))

(defn attach-path-to-refs
  [path form]
  (walk/postwalk (partial attach-path-if-ref path) form))

;; get properties
(defn properties
  [schema-file-path]
  (let [first-schema (attach-path-to-refs schema-file-path (slurp-yaml schema-file-path))]
    (loop [schemas [first-schema]
           props {}]
      (if (empty? schemas)
        props
        (let [schema (first schemas)
              current-path (:path schema)]
          (cond
            (contains? schema "properties")
            (recur (rest schemas) (into props (get schema "properties")))

            (contains? schema "allOf")
            (recur (into (rest schemas) (map (partial attach-path-if-ref current-path) (get schema "allOf"))) props)

            (ref? schema)
            (let [new-path (relative-to-file current-path (get schema "$ref"))
                  new-schema (assoc (slurp-yaml new-path) :path new-path)]
              (recur (conj (rest schemas) new-schema) props))))))))

(def component-specifications
  [{:name "Education"
    :generalizes #{"Program" "Course" "Component"}}

   {:name "Program"
    :file "schemas/Program.yaml"
    :extends "Education"
    :group "Educations"}
   {:name "Course"
    :file "schemas/Course.yaml"
    :extends "Education"
    :group "Educations"}
   {:name "Component"
    :file "schemas/Component.yaml"
    :extends "Education"
    :group "Educations"}

   {:name "Offering"
    :generalizes #{"ProgramOffering" "CourseOffering" "ComponentOffering"}}

   {:name "ProgramOffering"
    :file "schemas/ProgramOffering.yaml"
    :extends "Offering"
    :group "Offerings"}
   {:name "CourseOffering"
    :file "schemas/CourseOffering.yaml"
    :extends "Offering"
    :group "Offerings"}
   {:name "ComponentOffering"
    :file "schemas/ComponentOffering.yaml"
    :extends "Offering"
    :group "Offerings"}

   {:name "Result"
    :file "schemas/Result.yaml"}

   {:name "Association"
    :generalizes #{"ProgramOfferingAssociation" "CourseOfferingAssociation" "ComponentOfferingAssociation"}}
   {:name "ProgramOfferingAssociation"
    :file "schemas/ProgramOfferingAssociation.yaml"
    :extends "Association"}
   {:name "CourseOfferingAssociation"
    :file "schemas/CourseOfferingAssociation.yaml"
    :extends "Association"}
   {:name "ComponentOfferingAssociation"
    :file "schemas/ComponentOfferingAssociation.yaml"
    :extends "Association"}

   {:name "AcademicSession"
    :file "schemas/AcademicSession.yaml"}
   {:name "Building"
    :file "schemas/Building.yaml"}
   {:name "EducationSpecification"
    :file "schemas/EducationSpecification.yaml"}
   {:name "Group"
    :file "schemas/Group.yaml"}
   {:name "NewsFeed"
    :file "schemas/NewsFeed.yaml"}
   {:name "NewsItem"
    :file "schemas/NewsItem.yaml"}
   {:name "Organization"
    :file "schemas/Organization.yaml"}
   {:name "Person"
    :file "schemas/Person.yaml"}
   {:name "Room"
    :file "schemas/Room.yaml"}
   {:name "Service"
    :file "schemas/Service.yaml"}])

(defn components-by-name
  [components]
  (reduce
   (fn [m comp] (assoc m (:name comp) comp))
   {}
   components))

(defn add-properties-from-file
  [component]
  (if-let [file (:file component)]
    (assoc component :properties (properties file))
    component))

(defn merge-generalization-properties
  [index component]
  (if-let [generalizes (:generalizes component)]
    (let [all-properties (map (fn [comp-name] (get-in index [comp-name :properties])) generalizes)
          merged-properties (apply merge all-properties)
          overlap (apply set/intersection (map (comp set keys) all-properties))
          properties (->> overlap
                          (map (fn [prop-name] [prop-name (get merged-properties prop-name)]))
                          (into {}))]
      (assoc component :properties properties))
    component))

(defn merge-all-generalization-properties
  [components]
  (let [index (components-by-name components)]
    (map (partial merge-generalization-properties index) components)))

(defn clean-extend-properties
  [index component]
  (if-let [extends (:extends component)]
    (let [remove-set (->> (get-in index [extends :properties])
                          keys
                          (into #{}))
          properties (:properties component)
          cleaned-properties (->> properties
                                  (remove (fn [[k _]] (contains? remove-set k)))
                                  (into {}))]
      (assoc component :properties cleaned-properties))
    component))

(defn clean-all-extend-properties
  [components]
  (let [index (components-by-name components)]
    (map (partial clean-extend-properties index) components)))

(def components
  (->> component-specifications
       (map add-properties-from-file)
       (merge-all-generalization-properties)
       (clean-all-extend-properties)
       (remove (fn [{:keys [properties]}] (empty? properties)))))

(def always-remove
  #{"academicSession" "building" "children" "component" "consumers" "coordinators" "course" "courseOffering" "educationSpecification" "ext" "newsFeeds" "organization" "parent" "program" "programOffering" "programs" "room" "year" "result"})

(def preferred-ordering
  ["academicSessionId" "associationId" "buildingId" "componentId" "courseId" "educationSpecificationId" "groupId" "newsFeedId" "newsItemId" "offeringId" "organizationId" "personId" "programId" "roomId"
   "primaryCode" "otherCodes"
   "academicSessionType" "associationType" "componentType" "educationSpecificationType" "groupType" "newsFeedType" "newsItemType" "offeringType" "organizationType" "programType" "roomType"
   "name" "surname" "surnamePrefix" "givenName" "displayName" "titlePrefix" "titleSuffix"
   "shortName" "abbreviation"
   "description"
   "activeEnrollment"   "address"   "addresses"   "admissionRequirements"   "affiliations"   "assessment"   "authors"   "availableSeats"   "cityOfBirth"   "contactEmail"   "content"
   "countryOfBirth"   "dateOfBirth"   "dateOfNationality"   "documentation"   "duration"   "endDate"   "endDateTime"   "enrolledNumberStudents"
   "enrollEndDate"   "enrollment"   "enrollStartDate"   "fieldsOfStudy"   "firstStartDate"   "flexibleEntryPeriodEnd"   "flexibleEntryPeriodStart"   "floor"   "formalDocument"
   "gender"   "geolocation"   "ICEName"   "ICEPhoneNumber"   "ICERelation"   "image"   "initials"   "languageOfChoice"   "lastModified"   "learningOutcomes"   "level"   "levelOfQualification"
   "link"   "logo"   "mail"   "maxNumberStudents"   "minNumberStudents"   "mobileNumber"   "modeOfDelivery"   "modeOfStudy"   "nationality"   "office"   "pendingNumberStudents"
   "personCount"   "photoOfficial"   "photoSocial"   "priceInformation"   "qualificationAwarded"   "qualificationRequirements"   "remoteState"   "resources"   "result"   "resultExpected"   "resultValueType"   "resultWeight"
   "role"   "secondaryMail"   "sector"   "specification"   "startDate"   "startDateTime"   "state"   "studyLoad"   "teachingLanguage"   "telephoneNumber"
   "totalSeats"   "validFrom"   "validTo"   "validUntil"   "wing"])

(def preferred-order
  (apply hash-map (interleave preferred-ordering (range))))

(def indent-str "    ")

(defn indent
  [s]
  (str/join "\n" (map #(str indent-str %) (str/split-lines s))))

(defn str-prop
  [[name attrs]]
  (str indent-str "* " name ": " (get attrs "type" "unknown")))

(defn render-component
  [component]
  (let [props (->> (:properties component)
                   (remove (comp always-remove key))
                   (sort-by (fn [[k _]] (get preferred-order k))))
        str-props (str/join "\n" (map str-prop props))]
    (str "class " (:name component) " {"
         "\n"
         str-props
         "\n"
         "}")))

(def relations
  [["EducationSpecification" "0..1 - 0..*" "Education"]
   ["EducationSpecification" "0..1 - 0..1" "EducationSpecification"]

   ["Program" "0..1 - 0..1" "Program"]
   ["Program" "-:>" "Education"]
   ["Course" "-:>" "Education"]
   ["Component" "-:>" "Education"]

   ["Education" "coordinator - " "Person"]

   ["ProgramOffering" "-:>" "Offering"]
   ["CourseOffering" "-:>" "Offering"]
   ["ComponentOffering" "-:>" "Offering"]
   ["Education" "1 - 0..*" "Offering"]
   ["Offering" "1 - 0..*" "Association"]
   ["Association" "1 - 0..*" "Person"]

   ["Offering" "-" "AcademicSession"]
   ["AcademicSession" "-" "AcademicSession"]

   ["ComponentOffering" "-" "Room"]


   ["Organization" "-" "EducationSpecification"]
   ["Organization" "-" "Education"]
   ["Organization" "-" "Offering"]
   ["Organization" "-" "Group"]
   ["Organization" "-" "Organization"]

   ["Building" "-" "Room"]

   ["Group" "-" "Person"]
   ["Group" "-" "Offering"]

   ["NewsFeed" "-" "NewsItem"]])

(defn render-relation
  [[from conn to]]
  (str "[" from "] " conn " [" to "]"))

(->> components
     (group-by :group)
     #_(map render-component)
     keys)

(defn render-group
  [components]
  (str "together {\n"
       (str/join "\n\n" (->> components
                             (map render-component)
                             (map indent)))

       "}"))

(render-group (val (first (group-by :group components))))


(defn render
  []
  (let [component-groups (group-by :group components)
        components (get component-groups nil)
        component-groups (dissoc component-groups nil)]

    (str "@startuml\nallowmixing\n\n"
         (str/join "\n\n" (map render-component components))
         "\n\n"
         #_(str/join "\n\n" (map render-relation relations))
         "\n@enduml")))

(spit "diagram.puml" (render))
