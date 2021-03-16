(data-paths
  (registers
    ((name c)
     (buttons ((name c<-+) (source (operation plus)))))
    ((name p)
     (buttons ((name p<-*) (source (operation mult))))))
  (operations
    ((name plus)
     (inputs (register c) (constant 1)))
    ((name mult)
     (inputs (register p) (register c)))
    ((name >)
     (inputs (register c) (constant )))))

(controller
  test)
