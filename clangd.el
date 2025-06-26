;; C/C++ - Clangd setup
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-J=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(after! dap-mode
  (require 'dap-cpptools))


(dap-register-debug-template
 "(gdb) Launch UP2210V3"
 (list :type "cppdbg"
       :request "launch"
       :name "(gdb) Launch"
       :program "${workspaceFolder}/bin/UP2210V3.out"
       :args []
       :stopAtEntry :json-false
       :cwd "${fileDirname}"
       :environment []
       :externalConsole :json-false
       :MIMode "gdb"
       :setupCommands [
                       (:description "Enable pretty-printing for gdb"
                        :text "-enable-pretty-printing"
                        :ignoreFailures t)
                       (:description "Set Disassembly Flavor to Intel"
                        :text "-gdb-set disassembly-flavor intel"
                        :ignoreFailures t)
                       ]))
