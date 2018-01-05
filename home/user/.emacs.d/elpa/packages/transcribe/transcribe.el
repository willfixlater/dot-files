;;; transcribe.el --- Package for audio transcriptions

;; Copyright 2014-2017  Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 1.5.2

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; REQUIRES:
;; -----------------------------
;; This module works without any requires, but in order to use the audio 
;; functions, you need to install the Emacs package "emms", by Joe Drew,
;; and the external program "mpg321", by Jorgen Schafer and Ulrik Jensen,
;; both under GPL licenses.
;;
;; USAGE:
;; -------------------------
;; Transcribe is a tool to make audio transcriptions for discourse analysis
;; in the classroom.
;; It allows the transcriber to control the audio easily while typing, as well as
;; automate the insertion of xml tags, in case the transcription protocol
;; include them.
;; The analysis functions will search for a specific structure
;; of episodes that can be automatically added with the macro NewEpisode.
;; The function expects the speech acts to be transcribed inside a turn xml
;; tag with the identifier of the speaker with optional move attribute.
;; Each speech act is spected inside a <l1> or <l2> tag, depending
;; on the language used by the person. The attributes expected are the
;; number of clauses that form the utterance, the number of errors the
;; transcriber observes, and the function of the speech act. The parser will
;; work even if some attributes are missing.
;;
;;
;; AUDIO COMMANDS
;; ------------------------------
;;     C-x C-p ------> Play audio file. You will be prompted for the name
;;                     of the file. The recommended format is mp2.
;;     <f5> ---------> Pause or play audio.
;;     C-x <right> --> seek audio 10 seconds forward.
;;     C-x <left> --->seek audio 10 seconds backward.
;;     <f8> ---------> seek interactively: positive seconds go forward and
;;                       negative seconds go backward
;;
;; XML TAGGING COMMANDS
;; --------------------------------------------------
;;     C-x C-n ------> Create new episode structure. This is useful in case your
;;                 xml file structure requires it.
;;     <f2> ---------> Interactively insert a function attribute in a speech act
;;                 (l1 or l2) tag.
;;     <f3> ---------> Interactively insert a move attribute in a turn (person) tag
;;     <f4> ---------> Interactively insert an attribute (any kind)
;;     <f9> ---------> Insert turn (person) tag. Inserts a move attribute.
;;     <f10> --------> Insert a custom tag. Edit the function to adapt to your needs.
;;     <f11> --------> Insert speech act tag in L1, with clauses, errors and function
;;                     attributes.
;;     <f12> --------> Insert speech act tag in L2, with clauses, errors and function
;;                     attributes.
;;
;; AUTOMATIC PARSING
;; -----------------------------------------------------
;;     C-x C-a ------> Analyses the text for measurments of performance.

;;; Code:

(require 'xml)

;; (if t (require 'emms-setup))
;; (require 'emms-player-mpd)
;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "6600")

(emms-standard)
(emms-default-players)
(if t (require 'emms-player-mpg321-remote))
(defvar emms-player-list)
(push 'emms-player-mpg321-remote emms-player-list)

(if t (require 'emms-mode-line));FIXME: isn't `emms-mode-line' autoloaded?
(emms-mode-line 1)
(if t (require 'emms-playing-time));FIXME: isn't `emms-playing-time' autoloaded?
(emms-playing-time 1)

(defvar transcribe-function-list '("initiating" "responding" "control" "expressive" "interpersonal"))
(defvar transcribe-move-list '("initiation" "response" "follow-up"))
(defvar transcribe-attribute-list '("clauses" "errors" "function" "move"))
;; (append transcribe-attribute-list transcribe-function-list transcribe-move-list)

(defun transcribe-analyze-episode (episode person)
  "This calls the external python package analyze_episodes2.py. The new
   function transcribe-analyze implements its role now."
  (interactive "sepisode: \nsperson:")
  (shell-command (concat (expand-file-name  "analyze_episodes2.py")
                  " -e " episode " -p " person " -i " buffer-file-name )))

(defun transcribe-raw-to-buffer ()
  "EXPERIMENTAL - Convert the xml tagged transcription to raw transcription, with the names
   and the persons and the utterances only. The raw transcription will be send to buffer called
   `Raw Output'."
  (interactive)
  (let* ((xml (xml-parse-region (point-min) (point-max)))
    (results (car xml))
    (episodes (xml-get-children results 'episode)))

    (dolist (episode episodes)
      (let* ((transcription (xml-get-children episode 'transcription)))

        (dolist (turn transcription)
          (dolist (intervention (xml-node-children turn))
            (if (listp intervention)
              (progn
                (with-current-buffer "Raw Output"
                  (insert (format "%s\t" (line-number-at-pos)))
                  (insert (format "%s:\t" (car intervention)))
                  (dolist (utterance (nthcdr 2 intervention))
                    (if (listp utterance)
                       (progn
                         (insert (format "%s "  (nth 2 utterance))))

                         (insert (format "%s" utterance))))))

                       (with-current-buffer "Raw Output"
                         (insert (format "%s" (line-number-at-pos)))
                         (insert (format "%s" intervention))))))))))

(defun transcribe-analyze (episodenumber personid)
  "Extract from a given episode and person the number of asunits per
   second produced, and the number of clauses per asunits, for L2 and L1.
   It writes two output files, one for L2 utterances and one for L1
   utterances, so that they can be used with external programs. Output will
   be inserted in `Statistics Output' buffer."
  (interactive "sepisodenumber: \nspersonid:")
  (let* ((interventionsl2 '())
     (interventionsl1 '())
     (xml (xml-parse-region (point-min) (point-max)))
     (results (car xml))
     (episodes (xml-get-children results 'episode))
     (asunitsl2 0.0000)
     (asunitsl1 0.0000)
     (shifts 0.0000);; TODO implement
     (initiating 0.0000)
     (responding 0.0000)
     (control 0.0000)
     (expressive 0.0000)
     (interpersonal 0.0000)
     (clausesl1 0.0000)
     ;; (errorsl1 0.0000);; TODO implement
     (clausesl2 0.0000)
     (errorsl2 0.0000)
     (duration nil)
     (role nil)
     (context nil)
     (demand nil)
     ;; (clausesmessage nil)
     (number nil))

     (dolist (episode episodes)
       (let*((numbernode (xml-get-children episode 'number))
         (tasknode (xml-get-children episode 'task)))

         (setq number (nth 2 (car numbernode)))
         (when (equal episodenumber number)
           (let* ((durationnode (xml-get-children episode 'duration))
             (transcription (xml-get-children episode 'transcription)))

             (setq duration (nth 2 (car durationnode)))

             (dolist (task tasknode)
              (let* ((rolenode (xml-get-children task 'role))
                (contextnode (xml-get-children task 'context))
                (demandnode (xml-get-children task 'demand)))

                (setq role (nth 2 (car rolenode)))
                (setq context (nth 2 (car contextnode)))
                (setq demand (nth 2 (car demandnode)))
                ;; (with-current-buffer "Statistics Output"
                   ;; (insert (format "role: %s; context: %s; demand: %s\n" role context demand)))
                ))

             (dolist (turn transcription)
               (let* ((interventionnode (xml-get-children turn
                 (intern personid))))

                 (dolist (intervention interventionnode)
                   (let* ((l2node (xml-get-children intervention 'l2))
                     (l1node (xml-get-children intervention 'l1)))

                     (dolist (l2turn l2node)
                       (let* ((l2 (nth 2 l2turn))
                          (attrs (nth 1 l2turn))
                          (clausesl2nodeinc (cdr (assq 'clauses attrs)))
                          (errorsl2inc (cdr (assq 'errors attrs)))
                          (function (cdr (assq 'function attrs))))

                          (when (string-equal function "initiating")
                            (setq initiating (+ initiating 1)))
                          (when (string-equal function "responding")
                            (setq responding (+ responding 1)))
                          (when (string-equal function "control")
                            (setq control (+ control 1)))
                          (when (string-equal function "expressive")
                            (setq expressive (+ expressive 1)))
                          (when (string-equal function "interpersonal")
                            (setq interpersonal (+ interpersonal 1)))
                          (when attrs
                            (setq clausesl2 (+ clausesl2 (string-to-number
                             clausesl2nodeinc)))
                            (setq errorsl2 (+ errorsl2 (string-to-number
                             errorsl2inc))))
                          (when l2
                            ;; (add-to-list 'interventionsl2 l2)
                            (when (string-match "@*" l2) (setq shifts (1+ shifts)))
                            (cl-pushnew l2 interventionsl2 :test #'equal)
                            (setq asunitsl2 (1+ asunitsl2)))))
                     (dolist (l1turn l1node)
                       (let*((l1 (nth 2 l1turn))
                         (clausesl1node (nth 1 l1turn))
                         (clausesl1nodeinc (cdr (car clausesl1node))))

                         (when (not (equal clausesl1node nil))
                           (setq clausesl1 (+ clausesl1 (string-to-number
                              clausesl1nodeinc))))
                         (when l1
                           ;; (add-to-list 'interventionsl1 l1)
                           (when (string-match "@*" l1) (setq shifts (1+ shifts)))
                           (cl-pushnew l1 interventionsl1 :test #'equal)
                           (setq asunitsl1 (1+ asunitsl1)))))))))))))
  ;; (reverse interventionsl2)
  ;; (write-region (format "%s" interventionsl2) nil (format "transcribe-output-%s-%s-l2.txt" episodenumber personid))
  ;; Write raw interventions to file will be supported by a different function
  ;; (reverse interventionsl1)
  ;; (write-region (format "%s" interventionsl1) nil (format "transcribe-output-%s-%s-l1.txt" episodenumber personid))
  ;; (print interventionsl2) ;uncomment to display all the interventions on screen
  (let((asunitspersecondl2 (/ asunitsl2 (string-to-number duration)))
    (clausesperasunitl2 (/ clausesl2 asunitsl2))
    (errorsperasunitl2 (/ errorsl2 asunitsl2))
    (asunitspersecondl1 (/ asunitsl1 (string-to-number duration)))
    ;; (clausesperasunitl1 (/ clausesl1 asunitsl1))
    (initiatingperasunitl2 (/ initiating asunitsl2))
    (respondingperasunitl2 (/ responding asunitsl2))
    (controlperasunitl2 (/ control asunitsl2))
    (expressiveperasunitl2 (/ expressive asunitsl2))
    (interpersonalperasunitl2 (/ interpersonal asunitsl2))
    (shiftsperasunit (/ shifts (+ asunitsl1 asunitsl2))))
    
    ;; Get rid of divisions by zero
    (when (= asunitsl2 0)
      (setq initiatingperasunitl2 0.0)
      (setq respondingperasunitl2 0.0)
      (setq controlperasunitl2 0.0)
      (setq expressiveperasunitl2 0.0)
      (setq interpersonalperasunitl2 0.0)
      (setq shiftsperasunit 0.0))

    ;; (princ clausesmessage)
    (princ (format "episode: %s, duration: %s, person: %s\n" episodenumber duration personid))
    (with-current-buffer "Statistics Output"
      (insert (format "%s,%s,%s,0,0,%s,%s,%s,%s,%s,QUAL-L2,%s,%s,%s,%s,%s,%s,aux,level,subject,yearofclil,month\n" personid episodenumber duration role context demand asunitspersecondl2 asunitspersecondl1 initiatingperasunitl2 respondingperasunitl2 controlperasunitl2 expressiveperasunitl2 interpersonalperasunitl2 shiftsperasunit)))
    (princ (format "L2(Asunits/second): %s, L2(clauses/Asunit): %s, L2(errors/Asunit):%s, L1(Asunits/second): %s\n"
          asunitspersecondl2 clausesperasunitl2 errorsperasunitl2 asunitspersecondl1))
    (princ (format "Functions/unit: Initiating: %s, Responding: %s, Control: %s, Expressive: %s, Interpersonal: %s" initiatingperasunitl2 respondingperasunitl2 controlperasunitl2 expressiveperasunitl2 interpersonalperasunitl2)))))

(defun transcribe-analyze-all ()
  "Analyze all file and output to `Statistics Output' buffer. The buffer will
   lost all previous data. The data in the buffer can be saved to a file and be
   passed to R for statistical analysis."
  (interactive)
  (let* ((xml (xml-parse-region (point-min) (point-max)))
     (results (car xml))
     (episodes (xml-get-children results 'episode)))
  
    (with-current-buffer "Statistics Output"
       (erase-buffer)
       (insert "person,episode,duration,C-UNITS(L2),C-UNITS(L1),role,context,demand,QUAN-L2,QUAN-L1,QUAL-L2,initiating,responding,control,expressive,interpersonal,shifts,aux,level,subjects,yearofCLIL,month\n"))
     (dolist (episode episodes)
       (let* ((numbernode (xml-get-children episode 'number))
         (participantsnode (xml-get-children episode 'participants))
         ;; (transcription (xml-get-children episode 'transcription))
         (number (nth 2 (car numbernode)))
         (participantsstring (nth 2 (car participantsnode)))
         (participants (split-string participantsstring)))

         (dolist (participant participants)
           (transcribe-analyze number participant))))))


(define-skeleton transcribe-xml-tag-person
  "Insert a speaker xml tag and move point accordingly."
  "Person: "
  "<" str " move=\"" (completing-read "Move: " transcribe-move-list)
  "\">" _ "</" str ">")

(define-skeleton transcribe-xml-tag
  "Encapsulate the marked region in the given tag."
  "Tag: "
  "<" str ">" _ "</" str ">")
(define-obsolete-function-alias 'transcribe-region-xml-tag
  #'transcribe-xml-tag "1.6")

(define-skeleton transcribe-add-attribute
  "Add an xml attribute at point with the name and value specified."
  (completing-read "Attribute name: " transcribe-attribute-list)
  ;; FIXME: provide more specific value completion depending on the
  ;; chosen attribute.
  str "=\"" '(read-string "Value: ") "\"")

(define-skeleton transcribe-add-attribute-function
  "Add the xml attribute `function' at point with the name specified."
  (completing-read "Function name: " transcribe-function-list)
  "function=\"" str "\"")

(define-skeleton transcribe-add-attribute-move
  "Add the xml attribute `move' at point with the name specified."
  (completing-read "Move name: " transcribe-move-list)
  "move=\"" str "\"")

(define-skeleton transcribe-xml-tag-l1
  "Insert an l1 tag and places the cursor."
  (completing-read "Function: " transcribe-function-list)
  '(re-search-forward "</l.>" (line-end-position) t)
  "<l1 clauses=\"1\" errors=\"0\" function=\"" str "\">" _ "</l1>")

(define-skeleton transcribe-xml-tag-l2
  "Insert a l2 tag and place the cursor."
  (completing-read "function:" transcribe-function-list)
  '(re-search-forward "</l.>" (line-end-position) t)
  "<l2 clauses=\"1\" errors=\"0\" function=\"" str "\">" _ "</l2>")

(define-skeleton transcribe-xml-tag-break
  "Break a unit into two.
That is, insert a closing and an opening tag."
  "Tag: "
  ;; FIXME: Auto-compute the tag rather than pestering the user!
  ;; Maybe we could simply use `nxml-split-element', for example.
  "</" str "><" str ">")

(defun transcribe-display-audio-info ()
  (interactive)
  (emms-player-mpg321-remote-proc)
  (shell-command "/usr/bin/mpg321 -R - &"))


(fset 'NewEpisode
      "<episode>\n<number>DATE-NUMBER</number>\n<duration></duration>\n<comment></comment>\n<subject>Subject (level)</subject>\n<participants></participants>\n<task>\n\t<role>low or high</role>\n<context>low or high</context>\n<demand>low or high</demand>\r</task>\n<auxiliar>Yes/no</auxiliar>\n<transcription>\n</transcription>\n</episode>");Inserts a new episode structure


(defvar transcribe-mode-map
   (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-p") 'emms-play-file)
    (define-key map (kbd "C-x C-a") 'transcribe-analyze)
    (define-key map (kbd "C-x C-n") 'NewEpisode)
    (define-key map (kbd "C-x <down>") 'emms-stop)
    (define-key map (kbd "C-x <right>") 'emms-seek-forward)
    (define-key map (kbd "C-x <left>") 'emms-seek-backward)
    (define-key map (kbd "<f2>") 'transcribe-add-attribute-move)
    (define-key map (kbd "<f3>") 'transcribe-add-attribute-function)
    (define-key map (kbd "<f4>") 'transcribe-add-attribute)
    (define-key map (kbd "<f5>") 'emms-pause)
    (define-key map (kbd "<f8>") 'emms-seek)
    (define-key map (kbd "<f9>") 'transcribe-xml-tag)
    (define-key map (kbd "<f10>") 'transcribe-xml-tag-person)
    (define-key map (kbd "<f11>") 'transcribe-xml-tag-l1)
    (define-key map (kbd "<f12>") 'transcribe-xml-tag-l2)
    map)
  "Keymap for Transcribe minor mode.")


(easy-menu-define transcribe-mode-menu transcribe-mode-map
  "Menu for Transcribe mode"
  '("Transcribe"
    ["Raw Output" transcribe-raw-to-buffer]
    "---"
    ["Analyze" transcribe-analyze]
    ["Analyze all" transcribe-analyze-all]
    "---"
    ["Add transcription header" NewEpisode]
    ["Add move attribute" transcribe-add-attribute-move]
    ["Add function attribute" transcribe-add-attribute-function]
    ["Add L1 intervention" transcribe-xml-tag-l1]
    ["Add L2 intervention" transcribe-xml-tag-l2]
    ["Add move" transcribe-xml-tag-person]
    "---"
    ["Play audio file" emms-play-file]
    ))


;;;###autoload
(define-minor-mode transcribe-mode
 "Toggle transcribe-mode"
  nil
  " Trans"
  transcribe-mode-map
  (generate-new-buffer "Statistics Output")
  (generate-new-buffer "Raw Output")
;;  (with-current-buffer "Raw Output"
;;    (linum-mode t)
;;    (setq linum-format "%d "))
  (with-current-buffer "Statistics Output"
    ;; (insert "person,episode,duration,C-UNITS(L2),C-UNITS(L1),role,context,demand,QUAN-L2,QUAN-L1,QUAL-L2,segmented,aux,level,subjects,yearofCLIL,month\n")
  )
  ;; TODO: save the students present in transcription in list so that we can use that list for transcribe-analyze-all
)

(provide 'transcribe)

;;; transcribe.el ends here
