;;; ============================================================
;;;  DIMDIM.lsp  —  תוסף יצירת מידות מ-XLINE עם קרבה לשכבות
;;;  פקודות: DIMDIM , DIMDIMSET , DIMDIMUNGROUP
;;; ============================================================

(vl-load-com)

;;; ----- קבועים -----
(setq *DIMDIM-DICT*  "DIMDIM_SETTINGS")

;;; === מזהה מורשה — לשנות לפי מחשב היעד (ריק = ללא נעילה) ===
(setq *DIMDIM-LICENSE* "")

(defun ddim:check-license ( / id )
  (if (= *DIMDIM-LICENSE* "")
    t
    (progn
      (setq id (strcase (getenv "COMPUTERNAME")))
      (if (= id (strcase *DIMDIM-LICENSE*))
        t
        (progn
          (princ "\nהתוסף אינו מורשה על מחשב זה.")
          nil)))))

(defun c:DIMDIMID ( / id )
  (setq id (getenv "COMPUTERNAME"))
  (princ (strcat "\nמזהה מחשב: " id))
  (princ))

;;; ============================================================
;;;  הגדרות: קריאה / כתיבה
;;;  אינדקסים: 0=שכבה  1=מרחק-בסיס(1:50)  2=סגנון  3=קנה-מידה
;;;             4=cross-layers  5=near-layers  6=מכפיל-גובה-טקסט  7=צבע-XLINE
;;; ============================================================

(defun ddim:get-settings ( / dicts d xrec data res )
  (setq dicts (namedobjdict))
  (setq d (dictsearch dicts *DIMDIM-DICT*))
  (if d
    (progn
      (setq xrec (cdr (assoc -1 d)))
      (setq data (entget xrec))
      (setq res '())
      (foreach pair data
        (if (= 1 (car pair))
          (setq res (cons (cdr pair) res))))
      (reverse res))
    nil))

(defun ddim:put-settings ( lst / dicts xrec data )
  (setq dicts (namedobjdict))
  (if (dictsearch dicts *DIMDIM-DICT*)
    (dictremove dicts *DIMDIM-DICT*))
  (setq data (list '(0 . "XRECORD") '(100 . "AcDbXrecord")))
  (foreach s lst
    (setq data (append data (list (cons 1 s)))))
  (setq xrec (entmakex data))
  (dictadd dicts *DIMDIM-DICT* xrec)
  lst)

(defun ddim:default-settings ()
  (list "0" "100.0" "0" "50" "" "" "3.0" "7"))

;;; ============================================================
;;;  רשימת שכבות
;;; ============================================================

(defun ddim:layer-list ( / l name )
  (setq l '())
  (setq name (tblnext "LAYER" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "LAYER" nil)))
  (acad_strlsort l))

;;; ============================================================
;;;  רשימת סגנונות מידה
;;; ============================================================

(defun ddim:style-list ( / l name )
  (setq l '())
  (setq name (tblnext "DIMSTYLE" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "DIMSTYLE" nil)))
  (acad_strlsort l))


;;; ============================================================
;;;  מזהה ייחודי
;;; ============================================================

(if (not *DIMDIM-COUNTER*) (setq *DIMDIM-COUNTER* 0))
(defun ddim:newid ()
  (setq *DIMDIM-COUNTER* (1+ *DIMDIM-COUNTER*))
  (strcat (rtos (getvar "MILLISECS") 2 0) "-" (itoa *DIMDIM-COUNTER*)))

;;; ============================================================
;;;  יצירת גרופ
;;; ============================================================

(defun ddim:make-group ( ss grp-name / doc grps grp n objs i err )
  (setq err
    (vl-catch-all-apply
      '(lambda ()
         (setq doc (vla-get-activedocument (vlax-get-acad-object)))
         (setq grps (vla-get-groups doc))
         (setq grp (vla-add grps grp-name))
         (setq n (sslength ss))
         (setq objs (vlax-make-safearray vlax-vbObject (cons 0 (1- n))))
         (setq i 0)
         (while (< i n)
           (vlax-safearray-put-element objs i
             (vlax-ename->vla-object (ssname ss i)))
           (setq i (1+ i)))
         (vla-appenditems grp objs))
      nil))
  (if (vl-catch-all-error-p err)
    (princ (strcat "\nשגיאה ביצירת גרופ: " (vl-catch-all-error-message err)))))

;;; ============================================================
;;;  מציאת גרופ של ישות
;;; ============================================================

(defun ddim:find-group ( ent / doc grps found-grp ent-handle member-ent )
  (setq found-grp nil)
  (setq ent-handle (cdr (assoc 5 (entget ent))))
  (vl-catch-all-apply
    '(lambda ()
       (setq doc (vla-get-activedocument (vlax-get-acad-object)))
       (setq grps (vla-get-groups doc))
       (vlax-for grp grps
         (if (and (not found-grp)
                  (= (substr (vla-get-name grp) 1 5) "DDIM-"))
           (vlax-for member grp
             (if (and (not found-grp)
                      (vl-catch-all-apply
                        '(lambda ()
                           (setq member-ent (vlax-vla-object->ename member))
                           (= ent-handle (cdr (assoc 5 (entget member-ent)))))
                        nil))
               (setq found-grp grp))))))
    nil)
  found-grp)


;;; ============================================================
;;;  פיצול / איחוד רשימת שכבות
;;; ============================================================

(defun ddim:list-to-str ( lst )
  (if lst
    (apply 'strcat (mapcar '(lambda (s) (strcat s ";")) lst))
    ""))

(defun ddim:str-to-list ( str / pos res )
  (setq res '())
  (if (and str (> (strlen str) 0))
    (while (setq pos (vl-string-search ";" str))
      (if (> pos 0)
        (setq res (append res (list (substr str 1 pos)))))
      (setq str (substr str (+ pos 2)))))
  res)

(defun ddim:split-words ( str / pos res )
  (setq res '())
  (if (and str (> (strlen str) 0))
    (progn
      (setq str (vl-string-trim " " str))
      (while (> (strlen str) 0)
        (setq pos (vl-string-search " " str))
        (if pos
          (progn
            (if (> pos 0)
              (setq res (append res (list (substr str 1 pos)))))
            (setq str (vl-string-trim " " (substr str (+ pos 2)))))
          (progn
            (setq res (append res (list str)))
            (setq str ""))))))
  res)

;;; בדיקה אם item מכיל לפחות אחת מהמילים ב-toks
(defun ddim:word-match ( item toks / found )
  (setq found nil)
  (foreach tok toks
    (if (wcmatch (strcase item) (strcat "*" (strcase tok) "*"))
      (setq found t)))
  found)

;;; סינון רשימה לפי מילות חיפוש מופרדות ברווח (OR) — שיטה כללית
(defun ddim:filter-by-words ( lst search-str / toks )
  (setq toks (ddim:split-words search-str))
  (if (not toks)
    lst
    (vl-remove-if-not
      '(lambda (item) (ddim:word-match item toks))
      lst)))

;;; ============================================================
;;;  דיאלוג בחירת שכבה (sub-dialog)
;;; ============================================================

(defun ddim:pick-layer ( path lays / dclid2 res idx filt-lays )
  (setq dclid2 (load_dialog path))
  (setq res nil)
  (setq idx 0)
  (setq filt-lays lays)
  (if (new_dialog "layer_picker" dclid2)
    (progn
      (start_list "pick_layer")
      (mapcar 'add_list lays)
      (end_list)
      (set_tile "pick_layer" "0")

      ;; סינון לפי טקסט
      (action_tile "filter"
        (strcat
          "(setq _ftxt (get_tile \"filter\"))"
          "(setq filt-lays"
          "  (if (>= (strlen _ftxt) 3)"
          "    (ddim:filter-by-words lays _ftxt)"
          "    lays))"
          "(start_list \"pick_layer\")"
          "(if filt-lays (mapcar (quote add_list) filt-lays))"
          "(end_list)"
          "(set_tile \"pick_layer\" \"0\")"
          "(setq idx 0)"))

      (action_tile "pick_layer"
        "(setq idx (atoi (get_tile \"pick_layer\")))")
      (action_tile "accept"
        "(setq idx (atoi (get_tile \"pick_layer\")))(done_dialog 1)")
      (if (= 1 (start_dialog))
        (setq res (nth idx filt-lays)))))
  (unload_dialog dclid2)
  res)

;;; ============================================================
;;;  כתיבת DCL
;;; ============================================================

(defun ddim:write-dcl ( / f path )
  (setq path (vl-filename-mktemp "ddim" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "ddim_dlg : dialog {" f)
  (write-line "  label = \"הגדרות DIMDIM\";" f)
  (write-line "  : boxed_column { label = \"הגדרות כלליות\";" f)
  (write-line "    : popup_list { key=\"layer\"; label=\"שכבה\"; }" f)
  (write-line "    : popup_list { key=\"style\"; label=\"סגנון מידה\"; }" f)
  (write-line "    : edit_box { key=\"scale\"; label=\"קנה מידה\"; edit_width=13; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    : boxed_column { label = \"צור קו מידה כשמידה חוצה קווים בשכבת:\";" f)
  (write-line "      : list_box { key=\"cross_layers\"; height=6; allow_accept=false; }" f)
  (write-line "      : row {" f)
  (write-line "        : popup_list { key=\"cross_sel\"; }" f)
  (write-line "        : button { key=\"add_cross\"; label=\" + \"; fixed_width=true; width=6; }" f)
  (write-line "        : button { key=\"del_cross\"; label=\" - \"; fixed_width=true; width=6; }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "    : boxed_column { label = \"אם קו המידה לא חוצה — צור קו מידה עבור קווים בשכבות:\";" f)
  (write-line "      : list_box { key=\"near_layers\"; height=6; allow_accept=false; }" f)
  (write-line "      : row {" f)
  (write-line "        : popup_list { key=\"near_sel\"; }" f)
  (write-line "        : button { key=\"add_near\"; label=\" + \"; fixed_width=true; width=6; }" f)
  (write-line "        : button { key=\"del_near\"; label=\" - \"; fixed_width=true; width=6; }" f)
  (write-line "      }" f)
  (write-line "      : text { label = \"המרחק המקסימלי לקו שאינו חוצה — מוגדר לפי קנה מידה 1:50:\"; }" f)
  (write-line "      : edit_box { key=\"distance\"; edit_width=13; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column { label = \"הגדרות XLINE\";" f)
  (write-line "    : row {" f)
  (write-line "      : edit_box { key=\"xline_color\"; label=\"צבע\"; edit_width=7; }" f)
  (write-line "      : button { key=\"xline_color_pick\"; label=\" ... \"; fixed_width=true; width=6; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path)

;;; מיקום חלון ההגדרות — בין מרכז המסך לפינה ימנית-תחתונה
(defun ddim:dlg-screen-xy ( / app x y )
  (setq x -1  y -1)
  (vl-catch-all-apply
    '(lambda ()
       (setq app (vlax-get-acad-object))
       (setq x (fix (+ (vlax-get-property app 'Left)
                       (* (vlax-get-property app 'Width) 0.60))))
       (setq y (fix (+ (vlax-get-property app 'Top)
                       (* (vlax-get-property app 'Height) 0.55)))))
    nil)
  (list x y))

;;; ============================================================
;;;  פונקציית דיאלוג
;;; ============================================================

(defun ddim:dlg ( cur / dclid path res result lays styles cross-list near-list xline-color xy _x _y )
  (setq path (ddim:write-dcl))
  (setq lays   (ddim:layer-list))
  (setq styles (ddim:style-list))
  (setq res nil)

  (setq cross-list (ddim:str-to-list (nth 4 cur)))
  (setq near-list  (ddim:str-to-list (nth 5 cur)))

  (setq dclid (load_dialog path))
  (setq xy (ddim:dlg-screen-xy)  _x (car xy)  _y (cadr xy))
  (if (not (if (and (> _x 0) (> _y 0))
        (new_dialog "ddim_dlg" dclid "" _x _y)
        (new_dialog "ddim_dlg" dclid)))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))

  (start_list "layer")  (mapcar 'add_list lays)   (end_list)
  (start_list "style")  (mapcar 'add_list styles) (end_list)
  (set_tile "layer" (itoa (if (vl-position (nth 0 cur) lays)   (vl-position (nth 0 cur) lays)   0)))
  (set_tile "style" (itoa (if (vl-position (nth 2 cur) styles) (vl-position (nth 2 cur) styles) 0)))

  (start_list "cross_layers")
  (if cross-list (mapcar 'add_list cross-list))
  (end_list)
  (start_list "near_layers")
  (if near-list (mapcar 'add_list near-list))
  (end_list)
  (start_list "cross_sel") (mapcar 'add_list lays) (end_list) (set_tile "cross_sel" "0")
  (start_list "near_sel")  (mapcar 'add_list lays) (end_list) (set_tile "near_sel" "0")

  (set_tile "scale"    (nth 3 cur))
  (set_tile "distance" (nth 1 cur))
  (setq xline-color (if (nth 7 cur) (atoi (nth 7 cur)) 7))
  (set_tile "xline_color" (itoa xline-color))

  (action_tile "add_cross"
    (strcat
      "(setq _lay (nth (atoi (get_tile \"cross_sel\")) lays))"
      "(if (and _lay (not (member _lay cross-list)))"
      "  (progn"
      "    (setq cross-list (append cross-list (list _lay)))"
      "    (start_list \"cross_layers\")"
      "    (mapcar (quote add_list) cross-list)"
      "    (end_list)))"))

  (action_tile "del_cross"
    (strcat
      "(setq _di (atoi (get_tile \"cross_layers\")))"
      "(if (and cross-list (>= _di 0) (< _di (length cross-list)))"
      "  (progn"
      "    (setq _i 0)"
      "    (setq cross-list (vl-remove-if (quote (lambda (x) (= (setq _i (1+ _i)) (1+ _di)))) cross-list))"
      "    (start_list \"cross_layers\")"
      "    (if cross-list (mapcar (quote add_list) cross-list))"
      "    (end_list)))"))

  (action_tile "add_near"
    (strcat
      "(setq _lay (nth (atoi (get_tile \"near_sel\")) lays))"
      "(if (and _lay (not (member _lay near-list)))"
      "  (progn"
      "    (setq near-list (append near-list (list _lay)))"
      "    (start_list \"near_layers\")"
      "    (mapcar (quote add_list) near-list)"
      "    (end_list)))"))

  (action_tile "del_near"
    (strcat
      "(setq _di (atoi (get_tile \"near_layers\")))"
      "(if (and near-list (>= _di 0) (< _di (length near-list)))"
      "  (progn"
      "    (setq _i 0)"
      "    (setq near-list (vl-remove-if (quote (lambda (x) (= (setq _i (1+ _i)) (1+ _di)))) near-list))"
      "    (start_list \"near_layers\")"
      "    (if near-list (mapcar (quote add_list) near-list))"
      "    (end_list)))"))

  (action_tile "xline_color_pick"
    (strcat
      "(setq _c (acad_colordlg xline-color))"
      "(if _c (progn (setq xline-color _c) (set_tile \"xline_color\" (itoa _c))))"))

  (action_tile "accept"
    (strcat
      "(setq res (list"
      " (nth (atoi (get_tile \"layer\")) lays)"
      " (get_tile \"distance\")"
      " (nth (atoi (get_tile \"style\")) styles)"
      " (get_tile \"scale\")"
      " (ddim:list-to-str cross-list)"
      " (ddim:list-to-str near-list)"
      " \"3.0\""
      " (get_tile \"xline_color\")))"
      "(done_dialog 1)"))

  (setq result (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete path)
  res)

;;; ============================================================
;;;  גיאומטריה — חיתוך סגמנט עם XLINE
;;; ============================================================

(defun ddim:seg-xline-isect ( p1 p2 dir pos / x1 y1 x2 y2 t-val )
  (setq x1 (car p1) y1 (cadr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (if (= dir 'H)
    (if (and (/= y1 y2) (<= (* (- y1 pos) (- y2 pos)) 0.0))
      (progn
        (setq t-val (/ (- pos y1) (- y2 y1)))
        (list (+ x1 (* t-val (- x2 x1))) pos 0.0))
      nil)
    (if (and (/= x1 x2) (<= (* (- x1 pos) (- x2 pos)) 0.0))
      (progn
        (setq t-val (/ (- pos x1) (- x2 x1)))
        (list pos (+ y1 (* t-val (- y2 y1))) 0.0))
      nil)))

(defun ddim:dist-to-xline ( pt dir pos )
  (if (= dir 'H)
    (abs (- (cadr pt) pos))
    (abs (- (car pt) pos))))

(defun ddim:closest-on-seg ( p1 p2 dir pos )
  (if (<= (ddim:dist-to-xline p1 dir pos)
          (ddim:dist-to-xline p2 dir pos))
    p1 p2))

;;; ============================================================
;;;  חילוץ סגמנטים מישות
;;; ============================================================

(defun ddim:entity-segs ( ed / etype verts i segs )
  (setq etype (cdr (assoc 0 ed)))
  (setq segs '())
  (cond
    ((= etype "LINE")
     (setq segs (list (list (cdr (assoc 10 ed))
                            (cdr (assoc 11 ed))))))
    ((= etype "LWPOLYLINE")
     (setq verts '())
     (foreach pair ed
       (if (= 10 (car pair))
         (setq verts (append verts (list (cdr pair))))))
     (setq i 0)
     (while (< i (1- (length verts)))
       (setq segs (append segs
         (list (list (nth i verts) (nth (1+ i) verts)))))
       (setq i (1+ i)))
     (if (= 1 (cdr (assoc 70 ed)))
       (setq segs (append segs
         (list (list (last verts) (car verts))))))))
  segs)

;;; ============================================================
;;;  בדיקת ניצבות סגמנט לכיוון XLINE
;;; ============================================================

(defun ddim:seg-is-perp ( p1 p2 dir / dx dy )
  (setq dx (abs (- (car  p2) (car  p1))))
  (setq dy (abs (- (cadr p2) (cadr p1))))
  (if (= dir 'H)
    (< dx 1e-6)
    (< dy 1e-6)))

;;; ============================================================
;;;  מציאת נקודות חיתוך — שכבות חוצות
;;;  מחזיר: נקודות ממוקמות על ה-XLINE
;;; ============================================================

(defun ddim:find-cross-pts ( dir pos layers / ss i ent ed ipt pts )
  (setq pts '())
  (foreach lay layers
    (setq ss (ssget "X" (list (cons 8 lay))))
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq ed (entget ent))
          (foreach seg (ddim:entity-segs ed)
            (if (ddim:seg-is-perp (car seg) (cadr seg) dir)
              (progn
                (setq ipt (ddim:seg-xline-isect (car seg) (cadr seg) dir pos))
                (if ipt (setq pts (cons ipt pts))))))
          (setq i (1+ i))))))
  pts)

;;; ============================================================
;;;  מציאת נקודות קרובות — שכבות לא חוצות
;;;  מחזיר: נקודות ה-endpoint האמיתיות של האובייקטים
;;; ============================================================

(defun ddim:find-near-pts ( dir pos layers dist / ss i ent ed p1 p2 _d1 _d2 pts )
  (setq pts '())
  (foreach lay layers
    (setq ss (ssget "X" (list (cons 8 lay))))
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq ed (entget ent))
          (foreach seg (ddim:entity-segs ed)
            (setq p1 (car seg))
            (setq p2 (cadr seg))
            (if (not (ddim:seg-xline-isect p1 p2 dir pos))
              (progn
                (setq _d1 (ddim:dist-to-xline p1 dir pos))
                (setq _d2 (ddim:dist-to-xline p2 dir pos))
                (if (<= _d1 _d2)
                  (if (<= _d1 dist)
                    (setq pts (cons (list (car p1) (cadr p1) 0.0) pts)))
                  (if (<= _d2 dist)
                    (setq pts (cons (list (car p2) (cadr p2) 0.0) pts)))))))
          (setq i (1+ i))))))
  pts)

;;; ============================================================
;;;  סינון נקודות לתחום שני הקליקים
;;; ============================================================

(defun ddim:filter-in-range ( pts dir pt1 pt2 / min-v max-v _v )
  (if (= dir 'H)
    (progn
      (setq min-v (min (car pt1) (car pt2)))
      (setq max-v (max (car pt1) (car pt2))))
    (progn
      (setq min-v (min (cadr pt1) (cadr pt2)))
      (setq max-v (max (cadr pt1) (cadr pt2)))))
  (vl-remove-if
    '(lambda (pt)
       (setq _v (if (= dir 'H) (car pt) (cadr pt)))
       (or (< _v min-v) (> _v max-v)))
    pts))

;;; ============================================================
;;;  מיון נקודות + הסרת כפילויות
;;; ============================================================

(defun ddim:sort-dedup ( pts dir pos tol / sorted res last-val val cur-best cur-best-d d )
  (setq sorted
    (vl-sort pts
      (if (= dir 'H)
        '(lambda (a b) (< (car a) (car b)))
        '(lambda (a b) (< (cadr a) (cadr b))))))
  (setq res '())
  (setq last-val -1.0e30)
  (setq cur-best nil)
  (setq cur-best-d 1.0e30)
  (foreach pt sorted
    (setq val (if (= dir 'H) (car pt) (cadr pt)))
    (setq d (ddim:dist-to-xline pt dir pos))
    (if (> (- val last-val) tol)
      (progn
        (if cur-best (setq res (append res (list cur-best))))
        (setq last-val val)
        (setq cur-best pt)
        (setq cur-best-d d))
      (if (< d cur-best-d)
        (progn
          (setq cur-best pt)
          (setq cur-best-d d)))))
  (if cur-best (setq res (append res (list cur-best))))
  res)

;;; ============================================================
;;;  קריאת גובה טקסט מסגנון מידה
;;; ============================================================

(defun ddim:get-dimtxt ( style / ds th )
  (setq ds (tblsearch "DIMSTYLE" style))
  (setq th (if ds (cdr (assoc 140 ds)) nil))
  (if (or (not th) (= th 0.0)) 2.5 th))

;;; ============================================================
;;;  לוגיקת Line — מחולצת לשימוש משותף ב-DIMDIM וב-QDIMS
;;; ============================================================

(defun ddim:run-line ( s /
                       dim-layer base-dist dim-style scale xline-color
                       cross-list near-list actual-dist
                       xr dir pos pt1 pt2
                       cross-pts near-pts all-pts
                       dim-pt grp-id )
  (setq dim-layer  (nth 0 s))
  (setq base-dist  (atof (nth 1 s)))
  (setq dim-style  (nth 2 s))
  (setq scale      (atof (nth 3 s)))
  (setq cross-list (ddim:str-to-list (nth 4 s)))
  (setq near-list  (ddim:str-to-list (nth 5 s)))
  (setq xline-color (if (nth 7 s) (atoi (nth 7 s)) 7))
  (setvar "DIMSCALE" scale)
  (setq actual-dist (* base-dist (/ scale 50.0)))
  (setq xr (ddim:pick-xline dim-layer xline-color))
  (if (not xr) (exit))
  (setq dir (car xr))
  (setq pos (cadr xr))
  (setq pt1 (caddr xr))
  (setq pt2 (cadddr xr))
  (setq cross-pts (ddim:find-cross-pts dir pos cross-list))
  (setq near-pts  (ddim:find-near-pts  dir pos near-list actual-dist))
  (setq cross-pts (ddim:filter-in-range cross-pts dir pt1 pt2))
  (setq near-pts  (ddim:filter-in-range near-pts  dir pt1 pt2))
  (setq all-pts (ddim:sort-dedup (append cross-pts near-pts) dir pos 0.1))
  (if (< (length all-pts) 2)
    (progn (princ "\nלא נמצאו מספיק נקודות ליצירת מידות.") (exit)))
  (princ (strcat "\nנמצאו " (itoa (length all-pts)) " נקודות."))
  (if (= dir 'H)
    (setq dim-pt (list (car pt1) pos 0.0))
    (setq dim-pt (list pos (cadr pt1) 0.0)))
  (setq grp-id (strcat "DDIM-" (ddim:newid)))
  (ddim:create-dims all-pts dir dim-style dim-layer dim-pt scale grp-id)
  (princ "\nהמידות נוצרו."))

;;; ============================================================
;;;  יצירת מידות לאורך XLINE
;;; ============================================================

(defun ddim:create-dims ( pts dir dim-style dim-layer dim-pt scale grp-id / i p1 p2 dir-kw new-ent ss )
  (setvar "CLAYER" dim-layer)
  (command "_.DIMSTYLE" "R" dim-style)
  (setvar "DIMSCALE" scale)
  (setq dir-kw (if (= dir 'H) "Horizontal" "Vertical"))
  (setq ss (ssadd))
  (setq i 0)
  (while (< i (1- (length pts)))
    (setq p1 (nth i pts))
    (setq p2 (nth (1+ i) pts))
    (command "_.DIMLINEAR"
      (list (car p1) (cadr p1))
      (list (car p2) (cadr p2))
      dir-kw
      (list (car dim-pt) (cadr dim-pt)))
    (setq new-ent (entlast))
    (if new-ent (ssadd new-ent ss))
    (setq i (1+ i)))
  (if (> (sslength ss) 0)
    (ddim:make-group ss grp-id)))

;;; ============================================================
;;;  מיקום XLINE — getpoint (תומך OSNAP) + לולאת אישור
;;; ============================================================

(defun ddim:pick-xline ( layer xline-color / pt1 pt2 dx dy ed xline-ent confirmed ans gr gr-code gr-pt *error* _snp )
  ;; טיפול בשגיאות — מוחק XLINE גם בביטול / Ctrl+C
  (setq *error*
    '(lambda (msg)
       (if xline-ent
         (vl-catch-all-apply '(lambda () (entdel xline-ent)) nil))
       (if (not (wcmatch msg "*break*,*cancel*,*exit*"))
         (princ (strcat "\nError: " msg)))
       (princ)))

  ;; קליק ראשון — עם OSNAP
  (setq pt1 (getpoint "\nבחר נקודת התחלה: "))
  (if (not pt1) (progn (princ "\nבוטל.") (exit)))

  ;; יצירת XLINE עזר
  (setq xline-ent
    (entmakex
      (list
        '(0 . "XLINE")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        (cons 62 xline-color)
        '(100 . "AcDbXline")
        (list 10 (car pt1) (cadr pt1) 0.0)
        '(11 1.0 0.0 0.0))))

  (if (not xline-ent)
    (progn (princ "\nשגיאה ביצירת XLINE.") (exit)))

  (setq confirmed nil  dx 1.0  dy 0.0)

  (while (not confirmed)
    ;; לולאת grread — תצוגה חיה של XLINE + snap על קליק
    (princ "\nבחר נקודת סיום: ")
    (setq pt2 nil)
    (while (not pt2)
      (setq gr      (grread t))
      (setq gr-code (car gr))
      (setq gr-pt   (cadr gr))
      (cond
        ((and (= gr-code 5) (listp gr-pt))
         (setq dx (abs (- (car  gr-pt) (car  pt1))))
         (setq dy (abs (- (cadr gr-pt) (cadr pt1))))
         (setq ed (entget xline-ent))
         (if (>= dx dy)
           (progn
             (setq ed (subst (list 10 (car  pt1) (cadr gr-pt) 0.0) (assoc 10 ed) ed))
             (setq ed (subst '(11 1.0 0.0 0.0)                     (assoc 11 ed) ed)))
           (progn
             (setq ed (subst (list 10 (car gr-pt) (cadr pt1) 0.0)  (assoc 10 ed) ed))
             (setq ed (subst '(11 0.0 1.0 0.0)                     (assoc 11 ed) ed))))
         (entmod ed)
         (entupd xline-ent))
        ((= gr-code 3)
         (setq _snp (vl-catch-all-apply '(lambda () (osnap gr-pt "END,MID,CEN,NOD,QUA,INT,INS,PER,TAN,NEA")) nil))
         (setq pt2 (if (and _snp (not (vl-catch-all-error-p _snp))) _snp gr-pt)))
        ((and (= gr-code 2) (= gr-pt 27))
         (entdel xline-ent) (setq xline-ent nil)
         (princ "\nבוטל.") (exit))
        ((= gr-code 25)
         (entdel xline-ent) (setq xline-ent nil)
         (princ "\nבוטל.") (exit))))

    ;; עדכון XLINE לנקודה שנבחרה
    (setq dx (abs (- (car  pt2) (car  pt1))))
    (setq dy (abs (- (cadr pt2) (cadr pt1))))
    (setq ed (entget xline-ent))
    (if (>= dx dy)
      (progn
        (setq ed (subst (list 10 (car  pt1) (cadr pt2) 0.0) (assoc 10 ed) ed))
        (setq ed (subst '(11 1.0 0.0 0.0)                   (assoc 11 ed) ed)))
      (progn
        (setq ed (subst (list 10 (car pt2) (cadr pt1) 0.0)  (assoc 10 ed) ed))
        (setq ed (subst '(11 0.0 1.0 0.0)                   (assoc 11 ed) ed))))
    (entmod ed)
    (entupd xline-ent)

    ;; שאלת אישור — אם לא, חוזר לבחירת נקודת סיום בלבד
    (initget "Yes No")
    (setq ans (getkword "\nהאם המיקום מתאים? [Yes/No]: "))
    (if (or (= ans "Yes") (not ans))
      (setq confirmed t)))

  ;; מחיקת קו העזר
  (entdel xline-ent)
  (setq xline-ent nil)

  ;; החזרה: כיוון / מיקום / pt1 / pt2
  (if (>= dx dy)
    (list 'H (cadr pt2) pt1 pt2)
    (list 'V (car pt2) pt1 pt2)))

;;; ============================================================
;;;  הפקודה הראשית DIMDIM
;;; ============================================================

(defun c:DIMDIM ( / s choice )
  (if (not (ddim:check-license)) (exit))
  (setq s (ddim:get-settings))
  (if (not s)
    (progn
      (princ "\nפעם ראשונה בקובץ - אנא הגדר את הפרמטרים.")
      (setq s (ddim:dlg (ddim:default-settings)))
      (if (not s) (setq s (ddim:default-settings)))
      (ddim:put-settings s)
      (princ "\nהגדרות נשמרו. הפעל DIMDIM שוב לצייר מידות.")
      (exit)))

  (setq choice (ddim:show-menu))
  (cond
    ((= choice 0) (princ "\nבוטל."))
    ((= choice 1) (ddim:run-line s))
    ((= choice 2)
     (setq s (ddim:dlg s))
     (if s (ddim:put-settings s))
     (princ "\nההגדרות נשמרו."))
    ((= choice 3) (c:DIMDIMUNGROUP)))

  (princ))

;;; ============================================================
;;;  DIMDIMSET
;;; ============================================================

(defun c:DIMDIMSET ( / s )
  (if (not (ddim:check-license)) (exit))
  (setq s (ddim:get-settings))
  (if (not s) (setq s (ddim:default-settings)))
  (setq s (ddim:dlg s))
  (if s (ddim:put-settings s))
  (princ "\nההגדרות נשמרו.")
  (princ))

;;; ============================================================
;;;  DIMDIMUNGROUP
;;; ============================================================

(defun c:DIMDIMUNGROUP ( / sel ent grp )
  (setq sel (entsel "\nבחר קו מידה: "))
  (if (not sel)
    (progn (princ "\nבוטל.") (exit)))
  (setq ent (car sel))
  (setq grp (ddim:find-group ent))
  (if (not grp)
    (progn (princ "\nהישות אינה שייכת לגרופ DIMDIM.") (exit)))
  (vl-catch-all-apply '(lambda () (vla-delete grp)) nil)
  (princ "\nהגרופ שוחרר.")
  (princ))

;;; ============================================================
;;;  DCL לתפריט המיני של QDIMS
;;; ============================================================

(defun ddim:write-menu-dcl ( / f path )
  (setq path (vl-filename-mktemp "qdim" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "qdims_menu : dialog {" f)
  (write-line "  label = \"\";" f)
  (write-line "  : button { key=\"btn_qdims\";    label=\"  QDims    \"; is_default=true; fixed_width=true; }" f)
  (write-line "  : button { key=\"btn_settings\"; label=\"  Settings  \"; fixed_width=true; }" f)
  (write-line "  : button { key=\"btn_ungroup\";  label=\"  Ungroup  \"; fixed_width=true; }" f)
  (write-line "  : button { key=\"btn_cancel\";   label=\"  Cancel   \"; is_cancel=true;  fixed_width=true; }" f)
  (write-line "}" f)
  (close f)
  path)

;;; ============================================================
;;;  מיקום חלון המיני ליד מרכז חלון התוכנה
;;; ============================================================

(defun ddim:menu-screen-xy ( / app x y )
  (setq x -1  y -1)
  (vl-catch-all-apply
    '(lambda ()
       (setq app (vlax-get-acad-object))
       (setq x (fix (+ (vlax-get-property app 'Left)
                       (* (vlax-get-property app 'Width) 0.45))))
       (setq y (fix (+ (vlax-get-property app 'Top)
                       (* (vlax-get-property app 'Height) 0.45)))))
    nil)
  (list x y))

;;; ============================================================
;;;  תפריט מיני — הצגה ובחירה (מחזיר 1/2/3 או 0 לביטול)
;;; ============================================================

(defun ddim:show-menu ( / path dclid result xy x y )
  (setq path (ddim:write-menu-dcl))
  (setq dclid (load_dialog path))
  (setq result 0)
  (setq xy (ddim:menu-screen-xy))
  (setq x (car xy))
  (setq y (cadr xy))
  (if (if (and (> x 0) (> y 0))
        (new_dialog "qdims_menu" dclid "" x y)
        (new_dialog "qdims_menu" dclid))
    (progn
      (action_tile "btn_qdims"    "(done_dialog 1)")
      (action_tile "btn_settings" "(done_dialog 2)")
      (action_tile "btn_ungroup"  "(done_dialog 3)")
      (action_tile "btn_cancel"   "(done_dialog 0)")
      (action_tile "cancel"       "(done_dialog 0)")
      (setq result (start_dialog)))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))
  (unload_dialog dclid)
  (vl-file-delete path)
  result)

;;; ============================================================
;;;  QDIMS — כניסה דרך כפתור סרגל הכלים
;;; ============================================================

(defun c:QDIMS ( / s choice )
  (if (not (ddim:check-license)) (exit))
  (setq s (ddim:get-settings))
  (if (not s)
    (progn
      (princ "\nפעם ראשונה — פותח הגדרות.")
      (c:DIMDIMSET))
    (progn
      (setq choice (ddim:show-menu))
      (cond
        ((= choice 0) (princ "\nבוטל."))
        ((= choice 1) (ddim:run-line s))
        ((= choice 2) (c:DIMDIMSET))
        ((= choice 3) (c:DIMDIMUNGROUP)))))
  (princ))

;;; ============================================================
;;;  יצירת סרגל כלים עם כפתור QDims
;;; ============================================================

(defun ddim:create-toolbar ( / app mgs mg tbs found-tb err )
  (setq err
    (vl-catch-all-apply
      '(lambda ()
         (setq app (vlax-get-acad-object))
         (setq mgs (vla-get-menugroups app))
         (setq mg (vla-item mgs 0))
         (setq tbs (vla-get-toolbars mg))
         (setq found-tb nil)
         (vlax-for tb tbs
           (if (= (strcase (vla-get-name tb)) "DIMDIM")
             (setq found-tb tb)))
         (if (not found-tb)
           (progn
             (setq found-tb (vla-add tbs "DIMDIM"))
             (vla-addtoolbarbutton found-tb 0 "QDims" "DIMDIM" "^C^CQDIMS ")
             (vla-put-visible found-tb :vlax-true)
             (princ "\nסרגל כלים DIMDIM נוצר."))))
      nil))
  (if (vl-catch-all-error-p err)
    (princ "\nסרגל כלים: הוסף כפתור QDims ידנית (פקודה: QDIMS).")))

(ddim:create-toolbar)
(princ "\n=== DIMDIM נטען. פקודות: DIMDIM , DIMDIMSET , DIMDIMUNGROUP , QDIMS ===")
(princ)
