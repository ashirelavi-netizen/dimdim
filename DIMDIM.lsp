;;; ============================================================
;;;  DIMDIM.lsp  —  תוסף יצירת מידות מ-XLINE עם קרבה לשכבות
;;;  פקודות: DIMDIM , DIMDIMSET , DIMDIMUNGROUP
;;; ============================================================

(vl-load-com)

;;; ----- קבועים -----
(setq *DIMDIM-DICT*  "DIMDIM_SETTINGS")
(setq *DIMDIM-XAPP*  "DIMDIM_PAIR")
(setq *DIMDIM-VER*   "1")

;;; ============================================================
;;;  הגדרות: קריאה / כתיבה
;;;  אינדקסים: 0=שכבה  1=מרחק-בסיס(1:50)  2=סגנון  3=קנה-מידה
;;;             4=cross-layers  5=near-layers  6=מכפיל-גובה-טקסט
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
  (list "0" "100.0" "0" "50" "" "" "3.0"))

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
;;;  טגון XData
;;; ============================================================

(defun ddim:tag ( ent grp-id / ed )
  (entmod
    (append (entget ent)
      (list (list -3
        (list *DIMDIM-XAPP*
          (cons 1000 *DIMDIM-VER*)
          (cons 1000 grp-id)))))))

;;; ============================================================
;;;  מזהה ייחודי
;;; ============================================================

(if (not *DIMDIM-COUNTER*) (setq *DIMDIM-COUNTER* 0))
(defun ddim:newid ()
  (setq *DIMDIM-COUNTER* (1+ *DIMDIM-COUNTER*))
  (strcat (rtos (getvar "MILLISECS") 2 0) "-" (itoa *DIMDIM-COUNTER*)))

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
          "  (if (= _ftxt \"\")"
          "    lays"
          "    (vl-remove-if-not"
          "      (quote (lambda (_l)"
          "        (wcmatch (strcase _l)"
          "          (strcat \"*\" (strcase _ftxt) \"*\"))))"
          "      lays)))"
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
  (write-line "    : edit_box { key=\"scale\"; label=\"קנה מידה\"; edit_width=10; }" f)
  (write-line "    : edit_box { key=\"multiplier\"; label=\"מרחק קו המידה (x גובה טקסט)\"; edit_width=10; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    : boxed_column { label = \"צור קו מידה כשמידה חוצה קווים בשכבת:\";" f)
  (write-line "      : list_box { key=\"cross_layers\"; height=4; allow_accept=false; }" f)
  (write-line "      : button { key=\"add_cross\"; label=\"+ הוסף שכבה\"; }" f)
  (write-line "    }" f)
  (write-line "    : boxed_column { label = \"אם קו המידה לא חוצה — צור קו מידה עבור קווים בשכבות:\";" f)
  (write-line "      : list_box { key=\"near_layers\"; height=4; allow_accept=false; }" f)
  (write-line "      : button { key=\"add_near\"; label=\"+ הוסף שכבה\"; }" f)
  (write-line "      : text { label = \"המרחק המקסימלי לקו שאינו חוצה — מוגדר לפי קנה מידה 1:50:\"; }" f)
  (write-line "      : edit_box { key=\"distance\"; edit_width=10; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (write-line "layer_picker : dialog {" f)
  (write-line "  label = \"בחר שכבה להוספה\";" f)
  (write-line "  : edit_box { key=\"filter\"; label=\"סנן:\"; edit_width=20; }" f)
  (write-line "  : list_box { key=\"pick_layer\"; height=8; allow_accept=true; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path)

;;; ============================================================
;;;  פונקציית דיאלוג
;;; ============================================================

(defun ddim:dlg ( cur / dclid path res result lays styles cross-list near-list )
  (setq path (ddim:write-dcl))
  (setq lays   (ddim:layer-list))
  (setq styles (ddim:style-list))
  (setq res nil)

  (setq cross-list (ddim:str-to-list (nth 4 cur)))
  (setq near-list  (ddim:str-to-list (nth 5 cur)))

  (setq dclid (load_dialog path))
  (if (not (new_dialog "ddim_dlg" dclid))
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

  (set_tile "scale"      (nth 3 cur))
  (set_tile "distance"   (nth 1 cur))
  (set_tile "multiplier" (if (nth 6 cur) (nth 6 cur) "3.0"))

  (action_tile "add_cross"
    (strcat
      "(setq _picked (ddim:pick-layer path lays))"
      "(if (and _picked (not (member _picked cross-list)))"
      "  (progn"
      "    (setq cross-list (append cross-list (list _picked)))"
      "    (start_list \"cross_layers\")"
      "    (mapcar (quote add_list) cross-list)"
      "    (end_list)))"))

  (action_tile "add_near"
    (strcat
      "(setq _picked (ddim:pick-layer path lays))"
      "(if (and _picked (not (member _picked near-list)))"
      "  (progn"
      "    (setq near-list (append near-list (list _picked)))"
      "    (start_list \"near_layers\")"
      "    (mapcar (quote add_list) near-list)"
      "    (end_list)))"))

  (action_tile "accept"
    (strcat
      "(setq res (list"
      " (nth (atoi (get_tile \"layer\")) lays)"
      " (get_tile \"distance\")"
      " (nth (atoi (get_tile \"style\")) styles)"
      " (get_tile \"scale\")"
      " (ddim:list-to-str cross-list)"
      " (ddim:list-to-str near-list)"
      " (get_tile \"multiplier\")))"
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
            (setq ipt (ddim:seg-xline-isect (car seg) (cadr seg) dir pos))
            (if ipt (setq pts (cons ipt pts))))
          (setq i (1+ i))))))
  pts)

;;; ============================================================
;;;  מציאת נקודות קרובות — שכבות לא חוצות
;;;  מחזיר: נקודות ה-endpoint האמיתיות של האובייקטים
;;; ============================================================

(defun ddim:find-near-pts ( dir pos layers dist / ss i ent ed cpt d pts )
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
            (if (not (ddim:seg-xline-isect (car seg) (cadr seg) dir pos))
              (progn
                (setq cpt (ddim:closest-on-seg (car seg) (cadr seg) dir pos))
                (setq d   (ddim:dist-to-xline cpt dir pos))
                (if (<= d dist)
                  ;; מחזיר את ה-endpoint האמיתי (לא הטלה)
                  (setq pts (cons (list (car cpt) (cadr cpt) 0.0) pts))))))
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

(defun ddim:sort-dedup ( pts dir tol / sorted res last-val val )
  (setq sorted
    (vl-sort pts
      (if (= dir 'H)
        '(lambda (a b) (< (car a) (car b)))
        '(lambda (a b) (< (cadr a) (cadr b))))))
  (setq res '())
  (setq last-val -1.0e30)
  (foreach pt sorted
    (setq val (if (= dir 'H) (car pt) (cadr pt)))
    (if (> (- val last-val) tol)
      (progn
        (setq res (append res (list pt)))
        (setq last-val val))))
  res)

;;; ============================================================
;;;  קריאת גובה טקסט מסגנון מידה
;;; ============================================================

(defun ddim:get-dimtxt ( style / ds th )
  (setq ds (tblsearch "DIMSTYLE" style))
  (setq th (if ds (cdr (assoc 140 ds)) nil))
  (if (or (not th) (= th 0.0)) 2.5 th))

;;; ============================================================
;;;  יצירת מידות לאורך XLINE
;;; ============================================================

(defun ddim:create-dims ( pts dir dim-style dim-layer dim-pt scale grp-id / i p1 p2 dir-kw new-ent )
  (setvar "CLAYER" dim-layer)
  (command "_.DIMSTYLE" "R" dim-style)
  (setvar "DIMSCALE" scale)
  (setq dir-kw (if (= dir 'H) "Horizontal" "Vertical"))
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
    (if (and new-ent grp-id)
      (ddim:tag new-ent grp-id))
    (setq i (1+ i))))

;;; ============================================================
;;;  מיקום XLINE — getpoint (תומך OSNAP) + לולאת אישור
;;; ============================================================

(defun ddim:pick-xline ( layer / pt1 pt2 dx dy ed xline-ent orig-ortho confirmed ans )
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
        '(100 . "AcDbXline")
        (list 10 (car pt1) (cadr pt1) 0.0)
        '(11 1.0 0.0 0.0))))

  (if (not xline-ent)
    (progn (princ "\nשגיאה ביצירת XLINE.") (exit)))

  ;; הפעלת ORTHOMODE — מבטיח אופקי/אנכי בלבד
  (setq orig-ortho (getvar "ORTHOMODE"))
  (setvar "ORTHOMODE" 1)

  (setq confirmed nil)

  (while (not confirmed)
    ;; קליק שני — עם OSNAP (getpoint)
    (setq pt2 (getpoint pt1 "\nבחר נקודת סיום: "))
    (if (not pt2)
      (progn
        (setvar "ORTHOMODE" orig-ortho)
        (entdel xline-ent)
        (princ "\nבוטל.")
        (exit)))

    ;; עדכון XLINE למיקום הסופי
    (setq dx (abs (- (car pt2) (car pt1))))
    (setq dy (abs (- (cadr pt2) (cadr pt1))))
    (setq ed (entget xline-ent))
    (if (>= dx dy)
      (progn
        (setq ed (subst (list 10 (car pt1) (cadr pt2) 0.0) (assoc 10 ed) ed))
        (setq ed (subst '(11 1.0 0.0 0.0) (assoc 11 ed) ed)))
      (progn
        (setq ed (subst (list 10 (car pt2) (cadr pt1) 0.0) (assoc 10 ed) ed))
        (setq ed (subst '(11 0.0 1.0 0.0) (assoc 11 ed) ed))))
    (entmod ed)
    (entupd xline-ent)

    ;; שאלת אישור
    (initget "Yes No")
    (setq ans (getkword "\nהאם המיקום מתאים? [Yes/No]: "))
    (if (or (= ans "Yes") (not ans))
      (setq confirmed t)))

  ;; שחזור ORTHOMODE + מחיקת קו העזר
  (setvar "ORTHOMODE" orig-ortho)
  (entdel xline-ent)

  ;; החזרה: כיוון / מיקום / pt1 / pt2
  (if (>= dx dy)
    (list 'H (cadr pt2) pt1 pt2)
    (list 'V (car pt2) pt1 pt2)))

;;; ============================================================
;;;  הפקודה הראשית DIMDIM
;;; ============================================================

(defun c:DIMDIM ( / s opt xr dir pos pt1 pt2
                    dim-layer base-dist dim-style scale multiplier
                    cross-list near-list actual-dist
                    cross-pts near-pts all-pts
                    text-height dim-offset dim-pt grp-id )
  (if (not (tblsearch "APPID" *DIMDIM-XAPP*))
    (regapp *DIMDIM-XAPP*))

  (setq s (ddim:get-settings))
  (if (not s)
    (progn
      (princ "\nפעם ראשונה בקובץ - אנא הגדר את הפרמטרים.")
      (setq s (ddim:dlg (ddim:default-settings)))
      (if (not s) (setq s (ddim:default-settings)))
      (ddim:put-settings s)
      (princ "\nהגדרות נשמרו. הפעל DIMDIM שוב לצייר מידות.")
      (exit)))

  (initget "Line Ungroup Settings")
  (setq opt (getkword "\n[Line/Ungroup/Settings] <Line>: "))
  (if (not opt) (setq opt "Line"))

  (cond
    ((= opt "Settings")
     (setq s (ddim:dlg s))
     (if s (ddim:put-settings s))
     (princ "\nההגדרות נשמרו.")
     (exit))

    ((= opt "Ungroup")
     (princ "\nבחר מידה להפרדה.")
     (exit))

    ((= opt "Line")
     ;; קריאת הגדרות
     (setq dim-layer  (nth 0 s))
     (setq base-dist  (atof (nth 1 s)))
     (setq dim-style  (nth 2 s))
     (setq scale      (atof (nth 3 s)))
     (setq cross-list (ddim:str-to-list (nth 4 s)))
     (setq near-list  (ddim:str-to-list (nth 5 s)))
     (setq multiplier (if (nth 6 s) (atof (nth 6 s)) 3.0))

     ;; קנה מידה — DIMSCALE
     (setvar "DIMSCALE" scale)

     ;; מרחק גילוי מתואם לקנה המידה
     (setq actual-dist (* base-dist (/ scale 50.0)))

     ;; שלב 1: מיקום XLINE
     (setq xr  (ddim:pick-xline dim-layer))
     (if (not xr) (exit))
     (setq dir (car xr))
     (setq pos (cadr xr))
     (setq pt1 (caddr xr))
     (setq pt2 (cadddr xr))

     ;; שלב 2: מציאת נקודות
     (setq cross-pts (ddim:find-cross-pts dir pos cross-list))
     (setq near-pts  (ddim:find-near-pts  dir pos near-list actual-dist))

     ;; סינון לתחום שני הקליקים
     (setq cross-pts (ddim:filter-in-range cross-pts dir pt1 pt2))
     (setq near-pts  (ddim:filter-in-range near-pts  dir pt1 pt2))

     ;; מיון והסרת כפילויות
     (setq all-pts (ddim:sort-dedup (append cross-pts near-pts) dir 0.1))

     (if (< (length all-pts) 2)
       (progn
         (princ "\nלא נמצאו מספיק נקודות ליצירת מידות.")
         (exit)))

     (princ (strcat "\nנמצאו " (itoa (length all-pts)) " נקודות."))

     ;; שלב 3: חישוב מיקום קו המידה אוטומטי
     ;; אופקי — מעל ה-XLINE / אנכי — מימין ל-XLINE
     (setq text-height (ddim:get-dimtxt dim-style))
     (setq dim-offset  (* text-height scale multiplier))
     (if (= dir 'H)
       (setq dim-pt (list (car pt1) (+ pos dim-offset) 0.0))
       (setq dim-pt (list (+ pos dim-offset) (cadr pt1) 0.0)))

     ;; שלב 4: יצירת מידות
     (setq grp-id (ddim:newid))
     (ddim:create-dims all-pts dir dim-style dim-layer dim-pt scale grp-id)
     (princ "\nהמידות נוצרו.")))

  (princ))

;;; ============================================================
;;;  DIMDIMSET
;;; ============================================================

(defun c:DIMDIMSET ( / s )
  (if (not (tblsearch "APPID" *DIMDIM-XAPP*))
    (regapp *DIMDIM-XAPP*))
  (setq s (ddim:get-settings))
  (if (not s) (setq s (ddim:default-settings)))
  (setq s (ddim:dlg s))
  (if s (ddim:put-settings s))
  (princ "\nההגדרות נשמרו.")
  (princ))

;;; ============================================================
;;;  DIMDIMUNGROUP
;;; ============================================================

(defun c:DIMDIMUNGROUP ( / ent-sel ed xd xd-list grp-id ss i cur-ent cur-ed cur-xd cur-xd-list all-ents action )
  (if (not (tblsearch "APPID" *DIMDIM-XAPP*))
    (regapp *DIMDIM-XAPP*))
  (princ "\nבחר מידה: ")
  (setq ent-sel (car (entsel)))
  (if (not ent-sel)
    (progn (princ "\nבוטל.") (exit)))

  ;; קריאת xdata מהמידה הנבחרת
  (setq ed (entget ent-sel (list *DIMDIM-XAPP*)))
  (setq xd (cadr (assoc -3 ed)))
  (if (not xd)
    (progn (princ "\nמידה זו אינה שייכת לקבוצת DIMDIM.") (exit)))
  (setq xd-list (cdr xd))
  (setq grp-id (if (>= (length xd-list) 2) (cdr (cadr xd-list)) nil))
  (if (not grp-id)
    (progn (princ "\nשגיאה בקריאת מזהה הקבוצה.") (exit)))

  ;; מציאת כל המידות מאותה קבוצה
  (setq all-ents '())
  (setq ss (ssget "X" (list (cons -3 (list *DIMDIM-XAPP*)))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq cur-ent (ssname ss i))
        (setq cur-ed (entget cur-ent (list *DIMDIM-XAPP*)))
        (setq cur-xd (cadr (assoc -3 cur-ed)))
        (if cur-xd
          (progn
            (setq cur-xd-list (cdr cur-xd))
            (if (and (>= (length cur-xd-list) 2)
                     (= grp-id (cdr (cadr cur-xd-list))))
              (setq all-ents (cons cur-ent all-ents)))))
        (setq i (1+ i)))))

  (princ (strcat "\nנמצאו " (itoa (length all-ents)) " מידות בקבוצה."))
  (initget "Delete Ungroup")
  (setq action (getkword "\n[Delete/Ungroup] <Ungroup>: "))
  (if (not action) (setq action "Ungroup"))

  (cond
    ((= action "Delete")
     (foreach e all-ents (entdel e))
     (princ "\nהקבוצה נמחקה."))
    (t
     (foreach e all-ents
       (entmod (append (entget e) (list (list -3 (list *DIMDIM-XAPP*))))))
     (princ "\nהמידות הופרדו.")))
  (princ))

(princ "\n=== DIMDIM נטען. פקודות: DIMDIM , DIMDIMSET , DIMDIMUNGROUP ===")
(princ)
