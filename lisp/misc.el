;;; misc --- do misc stuff
;;; Commentary:


;;; Code:

;; Do not pause on redisplay, obsolete since 24.5.
(if (version< emacs-version "24.5")
    (setq redisplay-dont-pause t))

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
;; On Mac, in case default isn't what we want:
;; (setq browse-url-generic-program (expand-file-name "~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
(cond (*is-a-mac* (setq browse-url-browser-function 'browse-url-default-macosx-browser))
      (t (progn (setq browse-url-browser-function 'browse-url-generic)
                (setq browse-url-generic-program "chromium-browser"))))
(setq browse-url-generic-args (list "--incognito"))

;; Indent with spaces instead of tabs
;(setq-default indent-tabs-mode nil)


;; Avoid GC when in minibuffer
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq default-gc-cons-threshold gc-cons-threshold)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold default-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (and (setq bef (thing-at-point 'word))
                  (not (ispell-word nil 'quiet))
                  (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


;; http://endlessparentheses.com/improving-emacs-file-name-completion.html
;; Ignore various file extensions when completing file/buffer names
;; (setq read-file-name-completion-ignore-case t) ;; seems to be default
(setq read-buffer-completion-ignore-case t)
(mapc (lambda (x) (add-to-list 'completion-ignored-extensions x)) '(".$$$"
        ".000" ".a" ".a26" ".a78" ".acn" ".acr" ".agdai" ".aif" ".alg"
        ".ali" ".aliases" ".annot" ".ap_" ".api" ".api-txt" ".apk"
        ".app" ".aps" ".autosave" ".aux" ".auxlock" ".avi"
        ".azurePubxml" ".bak" ".bbl" ".bcf" ".bck" ".beam" ".beams"
        ".bim.layout" ".bin" ".blg" ".booproj" ".bowerrc" ".box"
        ".bpi" ".bpl" ".brf" ".bs" ".build.csdef" ".byte" ".cachefile"
        ".c_date" ".cfg" ".cfgc" ".cgo1.go" ".cgo2.c" ".chi" ".chs.h"
        ".class" ".cma" ".cmi" ".cmo" ".cmp" ".cmx" ".cmxa" ".cmxs"
        ".crc" ".crs" ".csproj" ".css.map" ".cubin" ".d" ".dart.js"
        ".db" ".dbmdl" ".dbproj.schemaview" ".dcp" ".dcu" ".debug"
        ".debug.app" ".def" ".DEPLOYED" ".dex" ".dll" ".dmb"
        ".dotCover" ".DotSettings.user" ".dox" ".dpth" ".drc" ".drd"
        ".dres" ".dri" ".drl" ".dsk" ".dump" ".dvi" ".dylib" ".dyn_hi"
        ".dyn_o" ".ear" ".egg" ".EGG" ".egg-info" ".EGG-INFO" ".elc"
        ".elf" ".eml" ".end" ".eps" ".epub" ".err" ".exe" ".exp" ".ez"
        ".fasl" ".FASL" ".fatbin" ".fdb_latexmk" ".fff" ".fls" ".fmt"
        ".fmx" ".fyc" ".gcda" ".gch" ".gcno" ".gcov" ".gem"
        ".GhostDoc.xml" ".glg" ".glo" ".glob" ".gls" ".gpi" ".gpState"
        ".gpu" ".hex" ".hi" ".hmap" ".hp" ".hpp" ".ibc" ".identcache"
        ".idx" ".ii" ".ilg" ".ilk" ".ind" ".info" ".info.json" ".init"
        ".int" ".ipa" ".ist" ".jar" ".js.deps" ".js.map"
        ".kicad_pcb-bak" ".ko" ".l15" ".la" ".lai" ".ldf" ".lib"
        ".lisp-temp" ".lk" ".llb" ".lnk" ".lo" ".loa" ".local" ".lof"
        ".log" ".lol" ".lot" ".loT" ".lst" ".lvlibp" ".lvlps" ".ly2"
        ".maf" ".manifest" ".map" ".md5" ".mdf" ".mem" ".meta" ".mh"
        ".mid" ".midi" ".mmx" ".mo" ".mobi" ".moc" ".mod" ".mode1v3"
        ".mode2v3" ".moved-aside" ".mp3" ".msi" ".mtc" ".mw" ".native"
        ".nav" ".ncb" ".net" ".nlo" ".nupkg" ".o" ".obj" ".obsolete"
        ".ocx" ".opendb" ".opensdf" ".opp" ".opt" ".opx" ".opx.broken"
        ".os" ".out" ".part.js" ".pbxuser" ".pcd" ".pch" ".pdb" ".pdf"
        ".pdfsync" ".perspectivev3" ".pfx" ".pgc" ".pgd" ".pid"
        ".pidb" ".pidb.meta" ".plc" ".plg" ".pls" ".plt" ".plx" ".pot"
        ".prof" ".projdata" ".pro.user" ".ps" ".psess" ".ptx"
        ".publishproj" ".publishsettings" ".pubxml" ".pxp" ".pxt"
        ".pyc" ".pyg" ".pyo" ".pytxcode" ".qbs.user"
        ".qmlproject.user" ".rbc" ".rbuistate" ".rdl.data" ".rel"
        ".rlib" ".rsc" ".rsm" ".rsp" ".rst" ".run.xml" ".sagetex.py"
        ".sagetex.sage" ".sagetex.scmd" ".sap" ".sav" ".sbr" ".scc"
        ".sdf" ".seed" ".sig" ".skb" ".sln" ".sln.docstates" ".slo"
        ".snm" ".so" ".sol" ".sout" ".spec" ".src.rock" ".stat" ".stc"
        ".sts" ".suo" ".svclog" ".svd" ".sym" ".sympy" ".synctex"
        ".synctex.gz" ".synctex.gz(busy)" ".tar" ".tar.gz" ".tdo"
        ".tds" ".test" ".tgz" ".thm" ".tikz" ".tlb" ".tlh" ".tli"
        ".tmp" ".tmp_proj" ".toc" ".ttt" ".tvsconfig" ".unityproj"
        ".upa" ".upb" ".user" ".userosscache" ".userprefs" ".uxp"
        ".uxt" ".v.d" ".VisualState.xml" ".vo" ".vrb" ".vsp" ".vspscc"
        ".vspx" ".vssscc" ".war" ".wav" ".wikidoc" ".x86_64"
        ".xccheckout" ".xcodeproj" ".xcscmblueprint" ".xcuserstate"
        ".xdy" ".xojo_uistate" ".zip" "-pkg.el" "-autoloads.el"
        "Notes.bib" "auto/"))


(provide 'misc)
;;; misc.el ends here
