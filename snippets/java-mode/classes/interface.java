# -*- mode: snippet -*-
# name: interface
# key: interface
# expand-env: ((yas/indent-line 'fixed))
# --
${1:public} interface ${2:`(file-name-sans-extension (buffer-name))`}$3 {
  $0
}
