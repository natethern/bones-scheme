set disassembly-flavor intel

define dumpx
  x/8xg $arg0
end

handle SIGPWR nostop pass
handle SIGXCPU nostop pass

set output-radix 16

display/i $rip
display/x $rax
display/x $r11
