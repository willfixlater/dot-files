set-psreadlineoption -editmode emacs
set-psreadlineoption -bellstyle none

try { $null = gcm pshazz -ea stop; pshazz init 'default' } catch { }
