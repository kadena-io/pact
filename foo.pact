(module foo BAR

  (defcap BAR ()
    true)

  (defschema ree
    a:string
    b:string)

  (defschema reee
    r:object)

  (deftable reee-table:{reee})


  (defun f:string (id:string a:string b:string)
    (insert reee-table id
      { 'r : { 'a : a, 'b : b }
      }))

  (defun g:string (id:string label:string a:string)
    (with-read reee-table id
      { 'r := ree }

      (update reee-table id
        { 'r : (+ (build label a) ree) })
      ))

  (defun h:string (id:string)
    (with-read reee-table id
      { 'r := ree }

      (format "{}" [ree])
      ))

  (defun build:object{ree} (a:string b:string)
    { a : b })
)

(create-table reee-table)
